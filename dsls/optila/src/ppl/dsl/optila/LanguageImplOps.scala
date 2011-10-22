package ppl.dsl.optila

trait LanguageImplOps { this: OptiLA =>
  def optila_vectordistance_abs_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[A]
  def optila_vectordistance_euc_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[A]
  def optila_vectordistance_square_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[A]
  def optila_matrixdistance_abs_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]
  def optila_matrixdistance_euc_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]
  def optila_matrixdistance_square_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]

  def optila_randsample_matrix_impl[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean]): Rep[Matrix[A]]
  def optila_randsample_vector_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Rep[Int])(implicit b: VectorBuilder[A,VA]): Rep[VA]
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: OptiLACompiler with OptiLALift =>
  

  def optila_vectordistance_abs_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]) = {
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

  def optila_vectordistance_euc_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]) = {
    //Math.sqrt(((v1-v2) mmap {e => e*e}).sum)
    println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")
    v1(0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  }

  def optila_vectordistance_square_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]) = {
    val d = v1-v2
    (d*d).sum
  }

  def optila_matrixdistance_abs_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    (m1-m2).abs.sum
  }

  def optila_matrixdistance_euc_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")
    m1(0,0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  }

  def optila_matrixdistance_square_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    val d = m1-m2
    matrix_times(d,d).sum
  }

  // TODO: refactor to call sampleCollection
  def optila_randsample_matrix_impl[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean]): Rep[Matrix[A]] = {
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

  def optila_randsample_vector_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Rep[Int])(implicit b: VectorBuilder[A,VA]) = {
    val candidates = (0::v.length).mutable

    val sampledOut = b.alloc(0, v.isRow)
    val sampled = b.toIntf(sampledOut)
    for (i <- 0 until numSamples){
      val r = i + random(v.length-i)
      val idx = candidates(r)
      sampled += v(idx)

      // remove index r from consideration
      val t = candidates(r)
      candidates(r) = candidates(i)
      candidates(i) = t
    }

    sampledOut
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
