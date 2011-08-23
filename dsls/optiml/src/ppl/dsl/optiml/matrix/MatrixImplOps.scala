package ppl.dsl.optiml.matrix

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,MatrixRow}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml.{OptiMLExp, OptiMLCompiler, OptiMLLift, OptiML}

trait MatrixImplOps { this: OptiML =>
  def matrix_obj_fromseq_impl[A:Manifest](xs: Seq[Rep[Vector[A]]]): Rep[Matrix[A]]
  def matrix_obj_fromvec_impl[A:Manifest](xs: Rep[Vector[Vector[A]]]): Rep[Matrix[A]]
  def matrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]): Rep[Matrix[A]]
  def matrix_obj_identity_impl(w: Rep[Int]): Rep[Matrix[Double]]
  //def matrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  //def matrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randnf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]

  def matrix_apply_impl[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  //def matrix_getrow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int]): Rep[Vector[A]]
  //def matrix_getcol_impl[A:Manifest](m: Rep[Matrix[A]], col: Rep[Int]): Rep[Vector[A]]
  def matrix_slice_impl[A:Manifest](m: Rep[Matrix[A]], startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int]): Rep[Matrix[A]]
  def matrix_slicerows_impl[A:Manifest](m: Rep[Matrix[A]], start: Rep[Int], end: Rep[Int]): Rep[Matrix[A]]
  def matrix_updaterow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def matrix_equals_impl[A:Manifest](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Boolean]
  def matrix_transpose_impl[A:Manifest](m: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]): Rep[Unit]
  def matrix_repmat_impl[A:Manifest](m: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]
  def matrix_inverse_impl[A](m: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]]
  def matrix_minrow_impl[A:Manifest:Ordering:HasMinMax](m: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_maxrow_impl[A:Manifest:Ordering:HasMinMax](m: Rep[Matrix[A]]): Rep[Vector[A]]
  //def matrix_maprows_impl[A:Manifest,B:Manifest](m: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[Vector[B]]): Rep[Matrix[B]]
  //def matrix_foreachrow_impl[A:Manifest](m: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[Unit]): Rep[Unit]
  def matrix_filterrows_impl[A:Manifest](m: Rep[Matrix[A]], pred: Rep[MatrixRow[A]] => Rep[Boolean]): Rep[Matrix[A]]
  def matrix_multiply_impl[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_times_vector_impl[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def matrix_sigmoid_impl[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]]
  def matrix_sigmoidf_impl[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Float]]
  def matrix_sumcol_impl[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_grouprowsby_impl[A:Manifest,K:Manifest](x: Rep[Matrix[A]], pred: Rep[Vector[A]] => Rep[K]): Rep[Vector[Matrix[A]]]
  
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiMLCompiler with OptiMLLift =>
  

  ///////////////
  // kernels

  def matrix_obj_identity_impl(w: Rep[Int]) = Matrix.diag(w, Vector.ones(w))
  //def matrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols)
  //def matrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols)
  def matrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => 1. }
  def matrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => 1f}
  def matrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => random[Double] }
  def matrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => random[Float] }
  def matrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => randomGaussian }
  def matrix_obj_randnf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => randomGaussian.floatValue() }

  // FIXME: the above functions return mutable objects but they should be immutable

  def matrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]) = {
    val out = Matrix[A](w,w)
    var i = unit(0)
    while (i < w){
      out(i,i) = vals(i)
      i += 1
    }
    out.unsafeImmutable
  }

  def matrix_obj_fromseq_impl[A:Manifest](xs: Seq[Rep[Vector[A]]]): Rep[Matrix[A]] = {
    throw new UnsupportedOperationException("this is currently broken")
//    val m = Matrix[A](0,0)
//    for (i <- 0 until xs.length){
//      m += xs.applyRaw(i)
//    }
//    m
  }

  def matrix_obj_fromvec_impl[A:Manifest](xs: Rep[Vector[Vector[A]]]) = {
    if (xs.length == 0) {
      Matrix[A](0,0).unsafeImmutable
    }
    else {
      if (xs(0).isRow) {
        val numRows = xs.length
        val numCols = xs(0).length
        val out = Matrix[A](numRows, numCols)
        for (i <- 0 until numRows){
          for (j <- 0 until numCols){
            out(i,j) = xs(i)(j)
          }
        }
        out.unsafeImmutable
      }
      else {
        val numRows = xs(0).length
        val numCols = xs.length
        val out = Matrix[A](numRows, numCols)
        for (i <- 0 until numCols){
          for (j <- 0 until numRows){
            out(j,i) = xs(i)(j)
          }
        }
        out.unsafeImmutable
      }
    }

  }

  def matrix_apply_impl[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]) = {
    val offset = i*x.numCols+j
    dc_apply(x,offset)
  }

  //def matrix_getrow_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int]) = m.vview(row*m.numCols, 1, m.numCols, true)
  //def matrix_getcol_impl[A:Manifest](m: Rep[Matrix[A]], col: Rep[Int]) = m.vview(col, m.numCols, m.numRows, false)

  def matrix_slice_impl[A:Manifest](m: Rep[Matrix[A]], startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int]) = {
    //m.chkRange(beginrow, endrow)
    // Add check for col out of bounds
    // TODO: convert to view
    val out = Matrix[A](endRow-startRow, endCol-startCol)
    var i = startRow
    while (i < endRow) {
      var j = startCol
      while (j < endCol) {
        out(i-startRow, j-startCol) = m(i,j)
        j += 1
      }
      i += 1
    }
    out.unsafeImmutable
  }

  def matrix_slicerows_impl[A:Manifest](m: Rep[Matrix[A]], start: Rep[Int], end: Rep[Int]) = {
    //m.chkRange(start, end)
    val out = Matrix[A](end-start, m.numCols)
    var i = start
    while (i < end) {
      var j = unit(0)
      while (j < m.numCols) {
        out(i-start, j) = m(i,j)
        j += 1
      }
      i += 1
    }
    out.unsafeImmutable
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

  def matrix_equals_impl[A:Manifest](x: Rep[Matrix[A]], y: Rep[Matrix[A]]) = {
    if (x.numRows != y.numRows || x.numCols != y.numCols) {
      false
    }
    else {
      var foundDiff = false
      var i = 0
      var j = 0
      while (i < x.numRows && !foundDiff) {
        while (j < x.numCols && !foundDiff) {
          if (x(i,j) != y(i,j))
            foundDiff = true
          j += 1
        }
        j = 0
        i += 1
      }
      !foundDiff
    }
  }

  def matrix_transpose_impl[A:Manifest](m: Rep[Matrix[A]]) = {
    // naive, should block
    val out = Matrix[A](m.numCols, m.numRows)
    for (i <- 0 until out.numRows){
      for (j <- 0 until out.numCols){
        out(i,j) = m(j,i)
      }
    }
    out.unsafeImmutable
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
    out.unsafeImmutable
  }

  // TODO: try/catch, case, in embedded implementation? we need to lift these still.
  def matrix_inverse_impl[A](m: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]] = {
    //m.chkEquals(m.numCols, m.numRows)

    // augment the Matrix with the identity Matrix of the same size
    val id = Matrix.identity(m.numCols)
    val augMat = m.toDouble.mutable
    augMat.insertAllCols(augMat.numCols, id)
//    try{
      // perform row reductions
      val redMat = matrix_rreduce(augMat)
      // strip off the augmented Matrix
      redMat.removeCols(0, m.numCols)
      redMat.unsafeImmutable
//    }
//    catch{
//      case e: Exception => {
//        println("Error: Matrix appears to be singular")
//        throw new IllegalArgumentException
//      }
//    }
  }

   protected def matrix_rreduce(m: Rep[Matrix[Double]]): Rep[Matrix[Double]] = {
    // assumes m is mutable
    val currentMat = m
    var lead = unit(0)
    var finished = unit(false)
    var r = unit(0)

    while (!finished && r < m.numRows) {
      if (m.numRows <= lead){
        finished = true
      }
      if (!finished){
        var i = r
        while (!finished && currentMat(i, lead) == 0.0){
          i += 1
          if (m.numCols == i){
            i = r
            lead += 1
            if (m.numRows == lead) {
              finished = true
            }
          }
        }

        if (!finished){
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
      }
      r += 1
    }

    currentMat
  }

  def matrix_minrow_impl[A:Manifest:Ordering:HasMinMax](m: Rep[Matrix[A]]): Rep[Vector[A]] = {
    throw new UnsupportedOperationException("not implemented yet")
//    val sumRows = m.mapRowsToVec[B](row => row.sum[B])
//    val idx = sumRows.minIndex
//    m(idx).clone
  }

  def matrix_maxrow_impl[A:Manifest:Ordering:HasMinMax](m: Rep[Matrix[A]]): Rep[Vector[A]] = {
    throw new UnsupportedOperationException("not implemented yet")
//    val sumRows = mapRowsToVec[B](row => row.sum[B])
//    val idx = sumRows.maxIndex
//    m(idx).clone
  }

//  def matrix_maprows_impl[A:Manifest,B:Manifest](m: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[Vector[B]]) = {
//    val first = f(m.getRow(0))
//    val out = Matrix[B](m.numRows, first.length)
//    out.updateRow(0, first)
//
//    // this should be task parallel with deg control flow - except the reflectEffect orderings of updateRow will cause
//    // false serialization
//    for (i <- 1 until m.numRows){
//      out.updateRow(i, f(m.getRow(i)))
//    }
//    out
//  }

//  def matrix_foreachrow_impl[A:Manifest](m: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[Unit]) = {
//    for (i <- 0 until m.numRows){
//      f(m.getRow(i))
//    }
//  }

  def matrix_filterrows_impl[A:Manifest](m: Rep[Matrix[A]], pred: Rep[MatrixRow[A]] => Rep[Boolean]) = {
    val v = Vector[Vector[A]](0,true)
    for (i <- 0 until m.numRows){
      val vv = m.getRow(i)
      if (pred(vv))
        v += vv
    }
    Matrix[A](v)
  }

  def matrix_multiply_impl[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]] = {

    val yTrans = y.t
    val out = Matrix[A](x.numRows, y.numCols)

    for (rowIdx <- 0 until x.numRows) {
      var i = unit(0)
      while (i < out.numCols) {
        var j = unit(1)
        var acc = x(rowIdx, 0) * yTrans(i, 0)
        while (j < yTrans.numCols) {
          acc += x(rowIdx, j) * yTrans(i, j)
          j += 1
        }
        out(rowIdx, i) = acc
        i += 1
      }
    }
    out.unsafeImmutable
  }

  def matrix_times_vector_impl[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Vector[A]]): Rep[Vector[A]] = {
//  (0::x.numRows).t { rowIdx =>
//    x.getRow(rowIdx) *:* y
//  }

    val out = Vector[A](x.numRows, false)
    for (rowIdx <- 0 until x.numRows) {
      out(rowIdx) = x.getRow(rowIdx) *:* y
    }
    out.unsafeImmutable
  }

  def matrix_sigmoid_impl[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]] = {

    val out = x.map(in => (1.0/(1.0+Math.exp(conv(in)*(-1)))))
    out
  }

  def matrix_sigmoidf_impl[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Float]] = {

    val out = x.map(in => (1.0/(1.0+Math.exp(conv(in)*(-1)))).asInstanceOfL[Float])
    out
  }

  def matrix_sumcol_impl[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Vector[A]] = {
    val out = Vector[A](x.numCols,true)
    for(colIdx <- 0 until x.numCols) {
      out(colIdx) = x.getCol(colIdx).sum
    }
    out.unsafeImmutable
  }
  
  def matrix_grouprowsby_impl[A:Manifest,K:Manifest](x: Rep[Matrix[A]], pred: Rep[Vector[A]] => Rep[K]): Rep[Vector[Matrix[A]]]  = {
    val groups = HashMap[K,Matrix[A]]()
    
    var i = 0
    while (i < x.numRows) {
      val key = pred(x(i))      
      if (!(groups contains key)) {
        groups(key) = Matrix[A](0,0)        
      }
      groups(key) += x(i)
      i += 1
    }
  
    val out = Vector[Matrix[A]](0,true)
    for (m <- groups.values) {
      out += m.unsafeImmutable       
    }    
    out.unsafeImmutable
  }
    
}
