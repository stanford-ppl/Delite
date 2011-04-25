package ppl.dsl.optiml

import datastruct.scala.{Vertex, Edge, Graph, Vector, Matrix, Vertices}

trait LanguageImplOps { this: OptiML =>
  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A]
  def optiml_untilconverged_impl[V <:Vertex :Manifest, E <: Edge :Manifest](
     g: Rep[Graph[V, E]], block: Rep[V] => Rep[Unit]): Rep[Unit]

  def optiml_vectordistance_abs_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]): Rep[A]
  def optiml_vectordistance_euc_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]): Rep[A]
  def optiml_vectordistance_square_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]): Rep[A]
  def optiml_matrixdistance_abs_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]
  def optiml_matrixdistance_euc_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]
  def optiml_matrixdistance_square_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]

  def optiml_randsample_matrix_impl[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean]): Rep[Matrix[A]]
  def optiml_randsample_vector_impl[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int]): Rep[Vector[A]]
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: OptiMLCompiler with OptiMLLift =>
  
  def optiml_untilconverged_impl[V <: Vertex : Manifest, E <: Edge : Manifest](g: Rep[Graph[V, E]], block: Rep[V] => Rep[Unit]) = {
    val vertices = g.vertices

    val tasks = vertices.cloneL
    val seen = Set[V]()
    
    while(tasks.length > 0) {
      tasks.foreach(block)
      tasks.clear()
      var totalTasks = unit(0)
      
      for(i <- 0 until vertices.length) {
        val vtasks = vertices(i).tasks
        totalTasks += vtasks.length
        for(j <- 0 until vtasks.length) {
          val task = vtasks(j).asInstanceOfL[V]
          if(!seen.contains(task)) {
            tasks += task
            seen.add(task)
          }
        }

        vertices(i).clearTasks()
      }

      seen.clear()
    }
  }

  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A] = {

    var delta = unit(scala.Double.MaxValue)
    var prev = unit(null).asInstanceOfL[A]
    var next = x
    var iter = unit(0)

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.cloneL()
      else
        prev = next

//      try{
        next = block(next)
//      }
//      catch{
//        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
//      }
      iter += 1
      delta = diff(next, prev)
      //println("(" + delta + ")")
    }

      if (iter == max_iter){
        //throw new ConvergenceException("Maximum iterations exceeded")
        println("Maximum iterations exceeded")
        returnL()
      }

    next
  }


  def optiml_vectordistance_abs_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = {
    (v1-v2).abs.sum
/*
    var result = (v1(0) - v2(0)).abs
    var i = 1
    while (i < v1.length) {
      result += (v1(i) - v2(i)).abs
      i += 1
    }
    result
*/
  }

  def optiml_vectordistance_euc_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = {
    //Math.sqrt(((v1-v2) mmap {e => e*e}).sum)
    println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")
    v1(0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  }

  def optiml_vectordistance_square_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = {
    val d = v1-v2
    (d*d).sum
  }

  def optiml_matrixdistance_abs_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    (m1-m2).abs.sum
  }

  def optiml_matrixdistance_euc_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")
    m1(0,0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  }

  def optiml_matrixdistance_square_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    val d = m1-m2
    matrix_times(d,d).sum
  }

  // TODO: refactor to call sampleCollection
  def optiml_randsample_matrix_impl[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean]): Rep[Matrix[A]] = {
    val length = if (sampleRows) m.numRows else m.numCols
    val newRows = if (sampleRows) numSamples else m.numRows
    val newCols = if (sampleRows) m.numCols else numSamples

    val sampled = if(sampleRows) Matrix[A](0, newCols)
                  else Matrix[A](0,newRows) // transposed for efficiency

    val candidates = (0::length).mutable

    // transpose to make constructing sampling more efficient
    val mt = if (sampleRows) m else m.t

    for (i <- 0 until numSamples){
      val r = i + random(length-i)
      val idx = candidates(r)
      sampled += mt(idx).cloneL

      // remove index r from consideration
      val t = candidates(r)
      candidates(r) = candidates(i)
      candidates(i) = t
    }

    if (sampleRows) sampled else sampled.t
  }

  def optiml_randsample_vector_impl[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int]): Rep[Vector[A]] = {
    val candidates = (0::v.length).mutable

    val sampled = Vector[A](0, v.isRow)
    for (i <- 0 until numSamples){
      val r = i + random(v.length-i)
      val idx = candidates(r)
      sampled += v(idx)

      // remove index r from consideration
      val t = candidates(r)
      candidates(r) = candidates(i)
      candidates(i) = t
    }

    sampled
  }

  /*
  private def sampleCollection[A:Manifest](in: Rep[DeliteCollection[A]], out: Rep[DeliteCollection[A]], numSamples: Rep[Int]): Rep[DeliteCollection[A]] = {
    val candidates = (0::numSamples).cloneL // .mutable

    for (i <- 0 until numSamples){
      val r = i + random(in.size - i)
      val idx = candidates(r)
      sampled.dcUpdate(i, in.dcApply(idx))

      // remove index r from consideration
      val t = candidates(r)
      candidates(r) = candidates(i)
      candidates(i) = t
    }
  }
  */
}
