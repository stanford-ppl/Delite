package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

trait MatrixImplOps { this: OptiML =>
  def matrix_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[Rep[Vector[A]]]]): Rep[Matrix[A]]
  def matrix_obj_fromvec_impl[A:Manifest](xs: Rep[Vector[Vector[A]]]): Rep[Matrix[A]]
  def matrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]): Rep[Matrix[A]]
  def matrix_obj_identity_impl(w: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randnf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]

  def matrix_getrow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int]): Rep[Vector[A]]
  def matrix_getcol_impl[A:Manifest](m: Rep[Matrix[A]], col: Rep[Int]): Rep[Vector[A]]
  def matrix_slicerows_impl[A:Manifest](m: Rep[Matrix[A]], begin: Rep[Int], end: Rep[Int]): Rep[Matrix[A]]
  def matrix_updaterow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]): Rep[Unit]
  def matrix_repmat_impl[A:Manifest](m: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]
  def matrix_inverse_impl[A](m: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]]
  def matrix_transpose_impl[A:Manifest](m: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_minrow_impl[A:Manifest:Arith:Ordering](m: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_maxrow_impl[A:Manifest:Arith:Ordering](m: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_maprows_impl[A:Manifest,B:Manifest](m: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[Vector[B]]): Rep[Matrix[B]]
  def matrix_foreachrow_impl[A:Manifest](m: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[Unit]): Rep[Unit]
  def matrix_filterrows_impl[A:Manifest](m: Rep[Matrix[A]], pred: Rep[Vector[A]] => Rep[Boolean]): Rep[Matrix[A]]
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiML =>
  

  ///////////////
  // kernels

  def matrix_obj_identity_impl(w: Rep[Int]) = Matrix.diag(w, Vector.ones(w))
  def matrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols)
  def matrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols)
  def matrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => 1. }
  def matrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => 1f}
  def matrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => random[Double] }
  def matrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => random[Float] }
  def matrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => randomGaussian }
  def matrix_obj_randnf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => randomGaussian.floatValue() }

  def matrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]) = {
    val out = Matrix[A](w,w)
    var i = unit(0)
    while (i < w){
      out(i,i) = vals(i)
      i += 1
    }
    out
  }

  def matrix_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[Rep[Vector[A]]]]): Rep[Matrix[A]] = {
    throw new UnsupportedOperationException("this is currently broken")
//    val m = Matrix[A](0,0)
//    for (i <- 0 until xs.length){
//      m += xs.applyRaw(i)
//    }
//    m
  }

  def matrix_obj_fromvec_impl[A:Manifest](xs: Rep[Vector[Vector[A]]]) = {
    val numRows = xs.length
    val numCols = if (xs.length > 0) xs(0).length else unit(0)
    val out = Matrix[A](numRows, numCols)
    for (i <- 0 until numRows){
      for (j <- 0 until numCols){
        out(i,j) = xs(i)(j)
      }
    }
    out
  }

  def matrix_getrow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int]) = m.vview(row*m.numCols, 1, m.numCols, true)
  def matrix_getcol_impl[A:Manifest](m: Rep[Matrix[A]], col: Rep[Int]) = m.vview(col, m.numCols, m.numRows, false)

  def matrix_slicerows_impl[A:Manifest](m: Rep[Matrix[A]], begin: Rep[Int], end: Rep[Int]) = {
    //m.chkRange(begin, end)
    val out = Matrix[A](end-begin, m.numCols)
    var i = begin
    while (i < end) {
      var j = unit(0)
      while (j < m.numCols) {
        out(i-begin, j) = m(i,j)
        j += 1
      }
      i += 1
    }
    out
  }

  def matrix_updaterow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int], y: Rep[Vector[A]]) = {
    //chkEquals(x.length, numCols)
    // TODO: could be parallelized using a view
    var j = unit(0)
    while(j < m.numCols){
      m(row,j) = y(j)
      j += 1
    }
  }

  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]) = {
    for (i <- 0 until m.numRows){
      print("[ ")
      for (j <- 0 until m.numCols){
        print(m(i,j))
        print(" ")
      }
      print("]\\n")
    }
  }

  def matrix_repmat_impl[A:Manifest](m: Rep[Matrix[A]], iRep: Rep[Int], jRep: Rep[Int]) = {
    val out = Matrix[A](iRep*m.numRows, jRep*m.numCols)
    var i = unit(0)
    var j = unit(0)
    var ii = unit(0)
    var jj = unit(0)
    while(ii != iRep) {
      i = 0
      while(i != m.numRows) {
        jj = 0
        while(jj != jRep) {
          j = 0
          while(j != m.numCols) {
            out(ii*m.numRows+i, jj*m.numCols+j) = m(i,j)
            //index += 1
            j += 1
          }
          jj += 1
        }
        i += 1
      }
      ii += 1
    }
    out
  }

  // TODO: try/catch, case, in embedded implementation? we need to lift these still.
  def matrix_inverse_impl[A](m: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]] = {
    //m.chkEquals(m.numCols, m.numRows)

    var tmpMat = m.cloneL

    // augment the Matrix with the identity Matrix of the same size
    val id = Matrix.identity(m.numCols)
    val augMat = tmpMat.toDouble
    augMat.insertAllCols(tmpMat.numCols, id)
//    try{
      // perform row reductions
      val redMat = matrix_rreduce(augMat)
      // strip off the augmented Matrix
      redMat.removeCols(0, m.numCols)
      redMat
//    }
//    catch{
//      case e: Exception => {
//        println("Error: Matrix appears to be singular")
//        throw new IllegalArgumentException
//      }
//    }
  }

  protected def matrix_rreduce(m: Rep[Matrix[Double]]): Rep[Matrix[Double]] = {
    var currentMat = m
    var lead = unit(0)

    for (r <- 0 until m.numRows) {
      if (m.numRows <= lead)
        returnL(currentMat)

      var i = r
      while (currentMat(i, lead) == 0.0){
        i += 1
        if (m.numCols == i){
          i = r
          lead += 1
          if (m.numRows == lead)
            returnL(currentMat)
        }
      }

      val tmpRow = currentMat(i)
      currentMat(i) = currentMat(r)
      currentMat(r) = tmpRow

      currentMat(r) = repVecToVecOps(currentMat(r)) / currentMat(r,lead)

      for (i <- 0 until m.numRows){
        if (i != r)
          currentMat(i) = currentMat(i) - currentMat(r)*currentMat(i,lead)
      }
      lead += 1
    }

    currentMat
  }

  def matrix_transpose_impl[A:Manifest](m: Rep[Matrix[A]]) = {
    // naive, should block
    val out = Matrix[A](m.numCols, m.numRows)
    for (i <- 0 until out.numRows){
      for (j <- 0 until out.numCols){
        out(i,j) = m(j,i)
      }
    }
    out
  }

  def matrix_minrow_impl[A:Manifest:Arith:Ordering](m: Rep[Matrix[A]]): Rep[Vector[A]] = {
    throw new UnsupportedOperationException("not implemented yet")
//    val sumRows = m.mapRowsToVec[B](row => row.sum[B])
//    val idx = sumRows.minIndex
//    m(idx).clone
  }

  def matrix_maxrow_impl[A:Manifest:Arith:Ordering](m: Rep[Matrix[A]]): Rep[Vector[A]] = {
    throw new UnsupportedOperationException("not implemented yet")
//    val sumRows = mapRowsToVec[B](row => row.sum[B])
//    val idx = sumRows.maxIndex
//    m(idx).clone
  }

  def matrix_maprows_impl[A:Manifest,B:Manifest](m: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[Vector[B]]) = {
    val first = f(m.getRow(0))
    val out = Matrix[B](m.numRows, first.length)
    out.updateRow(0, first)

    // this should be task parallel with deg control flow - except the reflectEffect orderings of updateRow will cause
    // false serialization
    for (i <- 1 until m.numRows){
      out.updateRow(i, f(m.getRow(i)))
    }
    out
  }

  def matrix_foreachrow_impl[A:Manifest](m: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[Unit]) = {
    for (i <- 0 until m.numRows){
      f(m.getRow(i))
    }
  }

  def matrix_filterrows_impl[A:Manifest](m: Rep[Matrix[A]], pred: Rep[Vector[A]] => Rep[Boolean]) = {
    val v = Vector[Vector[A]](0,true)
    for (i <- 0 until m.numRows){
      val vv = m.getRow(i)
      if (pred(vv))
        v += vv
    }
    Matrix[A](v)
  }
}