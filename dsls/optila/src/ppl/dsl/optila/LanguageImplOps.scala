package ppl.dsl.optila

trait LanguageImplOps { this: OptiLA =>
  def optila_vectordistance_abs_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[A]
  def optila_vectordistance_euc_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[A]
  def optila_vectordistance_square_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[A]
  def optila_matrixdistance_abs_impl[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]]): Rep[A]
  def optila_matrixdistance_euc_impl[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]]): Rep[A]
  def optila_matrixdistance_square_impl[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]]): Rep[A]

  def optila_randsample_matrix_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean])(implicit b: MatrixBuilder[A,I,MA]): Rep[MA]
  def optila_randsample_vector_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Rep[Int])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def optila_randelem_impl[A:Manifest](v: Interface[Vector[A]]): Rep[A]
  
  def optila_matrix_determinant22_impl[A:Manifest:Arith](x: Interface[Matrix[A]]): Rep[A]
  def optila_matrix_determinant33_impl[A:Manifest:Arith](x: Interface[Matrix[A]]): Rep[A]
  def optila_matrix_determinant44_impl[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]): Rep[A]
  def optila_matrix_determinant_impl[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]): Rep[A]
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
    //sqrt(((v1-v2) mmap {e => e*e}).sum)
    println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")  // TODO AKS
    v1(0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  }

  def optila_vectordistance_square_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]) = {
    val d = v1-v2
    (d*d).sum
  }

  def optila_matrixdistance_abs_impl[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]]) = {
    (m1-m2).abs.sum
  }

  def optila_matrixdistance_euc_impl[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]]) = {
    println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED") // TODO AKS
    m1(0,0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  }

  def optila_matrixdistance_square_impl[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]]) = {
    val d = m1-m2
    (d*:*d).sum
  }

  // TODO: refactor to call sampleCollection
  def optila_randsample_matrix_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean])(implicit b: MatrixBuilder[A,I,MA]): Rep[MA] = {
    val length = if (sampleRows) m.numRows else m.numCols
    val newRows = if (sampleRows) numSamples else m.numRows
    val newCols = if (sampleRows) m.numCols else numSamples

    val sampledOut = if(sampleRows) b.alloc(0, newCols)
                  else b.alloc(0,newRows) // transposed for efficiency
    val sampled = b.toBuildableIntf(sampledOut)

    val candidates = (0::length).mutable

    // transpose to make constructing sampling more efficient
    val mt = b.toIntf(if (sampleRows) m.ops.elem.asInstanceOf[Rep[MA]] else m.t.ops.elem.asInstanceOf[Rep[MA]])

    for (i <- 0 until numSamples){
      val r = i + random(length-i)
      val idx = candidates(r)
      sampled += mt(idx).Clone

      // remove index r from consideration
      val t = candidates(r)
      candidates(r) = candidates(i)
      candidates(i) = t
    }

    if (sampleRows) b.finalizer(sampledOut) else b.toIntf(b.finalizer(sampledOut)).t.ops.elem.asInstanceOf[Rep[MA]]
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
    val candidates = (0::numSamples).Clone // .mutable

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
  
  def optila_randelem_impl[A:Manifest](v: Interface[Vector[A]]): Rep[A] = {
    val n = random(v.length-1)
    v(n)
  }
  
  def optila_matrix_determinant22_impl[A:Manifest:Arith](x: Interface[Matrix[A]]): Rep[A] = {
    x(0,0)*x(1,1)-x(0,1)*x(1,0)
  }
  
  def optila_matrix_determinant33_impl[A:Manifest:Arith](x: Interface[Matrix[A]]): Rep[A] = {
    x(0,0)*x(1,1)*x(2,2) + x(0,1)*x(1,2)*x(2,0) + x(0,2)*x(1,0)*x(2,1) -
    x(0,2)*x(1,1)*x(2,0) - x(0,1)*x(1,0)*x(2,2) - x(0,0)*x(1,2)*x(2,1)
  }
  
  def optila_matrix_determinant44_impl[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]): Rep[A] = {
    val two = 2.asInstanceOf[A]
    
    x(0,1)*x(0,1)*x(2,3)*x(2,3)     - x(2,2)*x(3,3)*x(0,1)*x(0,1)     + two*x(3,3)*x(0,1)*x(0,2)*x(1,2) -
    two*x(0,1)*x(0,2)*x(1,3)*x(2,3) - two*x(0,1)*x(0,3)*x(1,2)*x(2,3) + two*x(2,2)*x(0,1)*x(0,3)*x(1,3) + 
    x(0,2)*x(0,2)*x(1,3)*x(1,3)     - x(1,1)*x(3,3)*x(0,2)*x(0,2)     - two*x(0,2)*x(0,3)*x(1,2)*x(1,3) + 
    two*x(1,1)*x(0,2)*x(0,3)*x(2,3) + x(0,3)*x(0,3)*x(1,2)*x(1,2)     - x(1,1)*x(2,2)*x(0,3)*x(0,3) -
    x(0,0)*x(3,3)*x(1,2)*x(1,2)     + two*x(0,0)*x(1,2)*x(1,3)*x(2,3) - x(0,0)*x(2,2)*x(1,3)*x(1,3) -
    x(0,0)*x(1,1)*x(2,3)*x(2,3)     + x(0,0)*x(1,1)*x(2,2)*x(3,3)    
  }
    
  def optila_matrix_determinant_impl[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]): Rep[A] = {
    if (x.numRows == 2 && x.numCols == 2) optila_matrix_determinant22_impl(x)
    else if (x.numRows == 3 && x.numCols == 3) optila_matrix_determinant33_impl(x)
    else if (x.numRows == 4 && x.numCols == 4) optila_matrix_determinant44_impl(x)
    else {
      fatal("Matrix determinants for matrices > 4x4 is not implemented yet")
    }
  }
}
