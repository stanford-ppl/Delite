package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila._

trait DenseMatrixImplOps { this: OptiLA =>
  def densematrix_obj_fromseq_impl[A:Manifest](xs: Seq[Interface[Vector[A]]]): Rep[DenseMatrix[A]]
  def densematrix_obj_fromvec_impl[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]]): Rep[DenseMatrix[A]]
  def densematrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]]): Rep[DenseMatrix[A]]
  def densematrix_obj_identity_impl(w: Rep[Int]): Rep[DenseMatrix[Double]]
  //def densematrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  //def densematrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_randnf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  
  def densematrix_vview_impl[A:Manifest](x: Rep[DenseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVectorView[A]]
  def densematrix_apply_impl[A:Manifest](x: Rep[DenseMatrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def densematrix_update_impl[A:Manifest](x: Rep[DenseMatrix[A]], row: Rep[Int], col: Rep[Int], y: Rep[A]): Rep[Unit]
  def densematrix_rawapply_impl[A:Manifest](x: Rep[DenseMatrix[A]], idx: Rep[Int]): Rep[A]
  def densematrix_rawupdate_impl[A:Manifest](x: Rep[DenseMatrix[A]], idx: Rep[Int], y: Rep[A]): Rep[Unit]
  def densematrix_insertrow_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit]
  def densematrix_insertallrows_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Matrix[A]]): Rep[Unit]
  def densematrix_insertcol_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit]
  def densematrix_insertallcols_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Matrix[A]]): Rep[Unit]
  def densematrix_removerows_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def densematrix_removecols_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
    
  def densematrix_inverse_impl[A:Manifest](m: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double]): Rep[DenseMatrix[Double]]
  //def densematrix_multiply_impl[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]]): Rep[DenseMatrix[A]]
}

trait DenseMatrixImplOpsStandard extends DenseMatrixImplOps {
  this: OptiLACompiler with OptiLALift =>
  

  ///////////////
  // kernels

  def densematrix_obj_identity_impl(w: Rep[Int]) = DenseMatrix.diag(w, Vector.ones(w))
  //def densematrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Double](numRows, numCols)
  //def densematrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Float](numRows, numCols)
  def densematrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Double](numRows, numCols) mmap { e => 1. }
  def densematrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Float](numRows, numCols) mmap { e => 1f}
  def densematrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Double](numRows, numCols) mmap { e => random[Double] }
  def densematrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Float](numRows, numCols) mmap { e => random[Float] }
  def densematrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Double](numRows, numCols) mmap { e => randomGaussian }
  def densematrix_obj_randnf_impl(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix[Float](numRows, numCols) mmap { e => randomGaussian.floatValue() }

  // FIXME: the above functions return mutable objects but they should be immutable

  def densematrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]]) = {
    val out = DenseMatrix[A](w,w)
    var i = unit(0)
    while (i < w){
      out(i,i) = vals(i)
      i += 1
    }
    out.unsafeImmutable
  }

  def densematrix_obj_fromseq_impl[A:Manifest](xs: Seq[Interface[Vector[A]]]): Rep[DenseMatrix[A]] = {
    throw new UnsupportedOperationException("this is currently broken")
//    val m = DenseMatrix[A](0,0)
//    for (i <- 0 until xs.length){
//      m += xs.applyRaw(i)
//    }
//    m
  }

  def densematrix_obj_fromvec_impl[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]]) = {
    if (xs.length == 0) {
      DenseMatrix[A](0,0).unsafeImmutable
    }
    else {
      if (xs(0).isRow) {
        val numRows = xs.length
        val numCols = xs(0).length
        val out = DenseMatrix[A](numRows, numCols)
        for (i <- 0 until numRows){
          for (j <- 0 until numCols){
            val tmp = xs(i)
            out(i,j) = tmp(j)
          }
        }
        out.unsafeImmutable
      }
      else {
        val numRows = xs(0).length
        val numCols = xs.length
        val out = DenseMatrix[A](numRows, numCols)
        for (i <- 0 until numCols){
          for (j <- 0 until numRows){
            val tmp = xs(i)
            out(j,i) = tmp(j)
          }
        }
        out.unsafeImmutable
      }
    }

  }
  
  def densematrix_vview_impl[A:Manifest](x: Rep[DenseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]) = {
    DenseVectorView[A](densematrix_raw_data(x), start, stride, length, isRow)
  }
  
  def densematrix_apply_impl[A:Manifest](x: Rep[DenseMatrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A] = {
    val offset = i*x.numCols+j
    densematrix_rawapply_impl(x,offset)
  }
  
  def densematrix_update_impl[A:Manifest](x: Rep[DenseMatrix[A]], row: Rep[Int], col: Rep[Int], y: Rep[A]): Rep[Unit] = {
    val offset = row*x.numCols+col
    densematrix_rawupdate_impl(x,offset,y)
  }

  def densematrix_rawapply_impl[A:Manifest](x: Rep[DenseMatrix[A]], idx: Rep[Int]): Rep[A] = {
    val d = densematrix_raw_data(x)
    d(idx)
  }

  def densematrix_rawupdate_impl[A:Manifest](x: Rep[DenseMatrix[A]], idx: Rep[Int], y: Rep[A]): Rep[Unit] = {
    val d = densematrix_raw_data(x)
    array_unsafe_update(d,idx,y) //d(idx) = y
  }
  
  def densematrix_insertrow_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit] = {
    //chkEquals(y._length, _numCols)
    val idx = pos*x.numCols
    if (x.size == 0) densematrix_set_numcols(x, y.length)
    densematrix_insertspace(x, idx, x.numCols)
    val data = densematrix_raw_data(x)
    for (i <- idx until idx+x.numCols){
      array_unsafe_update(data,i,y(i-idx))
    }
    densematrix_set_numrows(x, x.numRows+1)
  }

  def densematrix_insertallrows_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], xs: Interface[Matrix[A]]): Rep[Unit] = {
    //chkEquals(xs._numCols, _numCols)
    val idx = pos*x.numCols
    if (x.size == 0) densematrix_set_numcols(x, xs.numCols)
    val sz = x.numCols*xs.numRows
    densematrix_insertspace(x, idx, sz)
    val data = densematrix_raw_data(x)
    for (i <- idx until idx+sz){
      array_unsafe_update(data,i,xs.dcApply(i-idx))
    }
    densematrix_set_numrows(x, x.numRows+xs.numRows)
  }

  def densematrix_insertcol_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit] = {
    //chkEquals(y._length, _numRows)
    val newCols = x.numCols+1
    if (x.size == 0) densematrix_set_numrows(x, y.length)    
    val outData = NewArray[A](x.numRows*newCols)
    for (i <- 0 until x.numRows){
      var col = 0
      for (j <- 0 until newCols) {
        if (j == pos){
          outData(i*newCols+j) = y(i)
        }
        else{
          outData(i*newCols+j) = x(i,col)
          col += 1
        }
      }
    }
    densematrix_set_raw_data(x, outData.unsafeImmutable)
    densematrix_set_numcols(x, newCols)
  }

  def densematrix_insertallcols_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], xs: Interface[Matrix[A]]): Rep[Unit] = {
    //m.chkEquals(xs._numRows, _numRows)
    val newCols = x.numCols+xs.numCols
    if (x.size == 0) densematrix_set_numrows(x, xs.numRows)
    val outData = NewArray[A](x.numRows*newCols)
    for (i <- 0 until x.numRows){
      var col = 0
      for (j <- 0 until newCols){
        if (j < pos || j >= pos+xs.numCols){
          outData(i*newCols+j) = x(i,col)
          col += 1
        }
        else{
          outData(i*newCols+j) = xs(i,j-pos)
        }
      }
    }
    densematrix_set_raw_data(x, outData.unsafeImmutable)
    densematrix_set_numcols(x, newCols)
  }

  def densematrix_removerows_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], num: Rep[Int]): Rep[Unit] = {
    val idx = pos*x.numCols
    val len = num*x.numCols
    val data = densematrix_raw_data(x)
    array_unsafe_copy(data, idx + len, data, idx, x.size - (idx + len))
    densematrix_set_numrows(x, x.numRows - num)
  }

  def densematrix_removecols_impl[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], num: Rep[Int]): Rep[Unit] = {
    val newCols = x.numCols-num
    val outData = NewArray[A](x.numRows*newCols)
    for (i <- 0 until x.numRows){
      var col = 0
      for (j <- 0 until x.numCols){
        if (j < pos || j >= pos+num){
          outData(i*newCols+col) = x(i,j)
          col += 1
        }
      }
    }
    densematrix_set_raw_data(x, outData.unsafeImmutable)
    densematrix_set_numcols(x, newCols)
  }

  protected def densematrix_ensureextra[A:Manifest](x: Rep[DenseMatrix[A]], extra: Rep[Int]): Rep[Unit] = {
    if (densematrix_raw_data(x).length - x.size < extra) {
      densematrix_realloc(x,x.size + extra)
    }
  }

  protected def densematrix_realloc[A:Manifest](x: Rep[DenseMatrix[A]], minLen: Rep[Int]): Rep[Unit] = {
    val data = densematrix_raw_data(x)
    var n = max(4, data.length * 2)
    while (n < minLen) n = n*2
    val d = NewArray[A](n)
    array_unsafe_copy(data, 0, d, 0, x.size)
    densematrix_set_raw_data(x, d.unsafeImmutable)
  }

  protected def densematrix_insertspace[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    if (pos < 0 || pos > x.size) fatal("IndexOutOfBounds")
    densematrix_ensureextra(x,len)
    val d = densematrix_raw_data(x)
    array_unsafe_copy(d, pos, d, pos + len, x.size - pos)
  }

  protected def densematrix_chkpos[A:Manifest](x: Rep[DenseMatrix[A]], index: Rep[Int]) = {
    if (index < 0 || index >= x.size) fatal("IndexOutOfBounds")
    index
  }  

  // def densematrix_multiply_impl[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]]): Rep[DenseMatrix[A]] = {
  // 
  //   val yTrans = y.t
  //   val out = DenseMatrix[A](x.numRows, y.numCols)
  // 
  //   for (rowIdx <- 0 until x.numRows) {
  //     var i = unit(0)
  //     while (i < out.numCols) {
  //       var j = unit(1)
  //       var acc = x(rowIdx, 0) * yTrans(i, 0)
  //       while (j < yTrans.numCols) {
  //         acc += x(rowIdx, j) * yTrans(i, j)
  //         j += 1
  //       }
  //       out(rowIdx, i) = acc
  //       i += 1
  //     }
  //   }
  //   out.unsafeImmutable
  // }
  
  // TODO: try/catch, case, in embedded implementation? we need to lift these still.
  def densematrix_inverse_impl[A:Manifest](m: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double]): Rep[DenseMatrix[Double]] = {
    //m.chkEquals(m.numCols, m.numRows)

    // augment the DenseMatrix with the identity DenseMatrix of the same size
    val id = DenseMatrix.identity(m.numCols)
    val isDouble: Boolean = manifest[A] == manifest[Double] // explicitly typed to prevent lifting
    val augMat = if (isDouble) (m.asInstanceOf[Rep[DenseMatrix[Double]]].mutable) else m.toDouble.mutable
    augMat.insertAllCols(augMat.numCols, id)
//    try{
      // perform row reductions
      val redMat = densematrix_rreduce(augMat)
      // strip off the augmented DenseMatrix
      redMat.removeCols(0, m.numCols)
      redMat.unsafeImmutable
//    }
//    catch{
//      case e: Exception => {
//        println("Error: DenseMatrix appears to be singular")
//        throw new IllegalArgumentException
//      }
//    }
  }

   protected def densematrix_rreduce(m: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]] = {
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
          val tmpRow = currentMat(i).Clone
          currentMat(i) = currentMat(r)
          currentMat(r) = tmpRow
          currentMat(r) = currentMat(r) / currentMat(r,lead)

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

  
  def densematrix_grouprowsby_impl[A:Manifest,K:Manifest](x: Rep[DenseMatrix[A]], pred: Rep[DenseVectorView[A]] => Rep[K]): Rep[DenseVector[DenseMatrix[A]]]  = {
    val groups = HashMap[K,DenseMatrix[A]]()
    
    var i = 0
    while (i < x.numRows) {
      val key = pred(x(i))      
      if (!(groups contains key)) {
        groups(key) = DenseMatrix[A](0,x.numCols).unsafeImmutable        
      }
      //groups(key) += x(i).Clone // AKS TODO: should not need clone
      groups(key) = groups(key) :+ x(i) // inefficient, but have to follow nested mutable rule
      i += 1
    }
  
    val out = DenseVector[DenseMatrix[A]](0,true)
    for (m <- groups.values) {
      out += m.unsafeImmutable       
    }    
    out.unsafeImmutable
  }
      
}
