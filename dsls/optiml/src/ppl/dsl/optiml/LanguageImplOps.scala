package ppl.dsl.optiml

trait LanguageImplOps { this: OptiML =>
  def optiml_untilconverged_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], block: Rep[Vertex[VD,ED]] => Rep[Unit]): Rep[Unit]
  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A]  
  // def optiml_triangular_impl(n: Rep[Int], includeDiagonal: Rep[Boolean]): (Rep[IndexVectorDense],Rep[IndexVectorDense])
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: OptiMLCompiler with OptiMLLift =>
  
  // def optiml_triangular_impl(n: Rep[Int], includeDiagonal: Rep[Boolean]) = {
  //   val cols = IndexVector(0, true)
  //   val rows = IndexVector(0, true)
  //   var i = 0
  //   var j = 0
  //   while (i < n) {
  //     var start = if (includeDiagonal) i else i+1
  //     var j = start
  //     while (j < n) {
  //       rows += i        
  //       cols += j
  //       j += 1
  //     }
  //     i += 1
  //   }
  //   (rows,cols)
  // }
  
  def optiml_untilconverged_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], block: Rep[Vertex[VD,ED]] => Rep[Unit]) = {
    val vertices = g.vertices

    val tasks = vertices.Clone
    //val tasks = repCloneableToCloneableOps(vertices)(vectorCloneable[V],manifest[Vertices[V]]).Clone
    val seen = Set[Vertex[VD,ED]]()
    
    while(tasks.length > 0) {
      tasks.foreach(block)
      tasks.clear()
      var totalTasks = unit(0)
      
      for(i <- 0 until vertices.length) {
        val vtasks = vertices(i).tasks
        totalTasks += vtasks.length
        for(j <- 0 until vtasks.length) {
          val task = vtasks(j).AsInstanceOf[Vertex[VD,ED]]
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
    var prev = unit(null).AsInstanceOf[A]
    var next = x
    var iter = unit(0)

    while ((abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.Clone()
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
}
