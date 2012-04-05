package ppl.dsl.optila.matrix

import ppl.dsl.optila.{DenseVector,Vector,VectorView,Matrix}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{OptiLAExp, OptiLACompiler, OptiLALift, OptiLA}

trait MatrixImplOps { this: OptiLA =>
  //def matrix_apply_impl[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def matrix_getrow_impl[A:Manifest](m: Interface[Matrix[A]], row: Rep[Int]): Rep[VectorView[A]]
  def matrix_getcol_impl[A:Manifest](m: Interface[Matrix[A]], col: Rep[Int]): Rep[VectorView[A]]
  def matrix_clone_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA]): Rep[MA]  
  def matrix_slice_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  def matrix_slicerows_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], start: Rep[Int], end: Rep[Int])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  def matrix_addrow_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], row: Interface[Vector[A]])(implicit b: MatrixBuilder[A,MA]): Rep[MA]  
  def matrix_updaterow_impl[A:Manifest](m: Interface[Matrix[A]], row: Rep[Int], y: Interface[Vector[A]]): Rep[Unit]
  def matrix_equals_impl[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]]): Rep[Boolean]
  def matrix_transpose_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  def matrix_pprint_impl[A:Manifest](m: Interface[Matrix[A]]): Rep[Unit]
  def matrix_repmat_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], i: Rep[Int], j: Rep[Int])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  //def matrix_inverse_impl[A](m: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]]
  def matrix_minrow_impl[A:Manifest:Ordering:HasMinMax,VA:Manifest](m: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def matrix_maxrow_impl[A:Manifest:Ordering:HasMinMax,VA:Manifest](m: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  //def matrix_maprows_impl[A:Manifest,B:Manifest](m: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[Vector[B]]): Rep[Matrix[B]]
  //def matrix_foreachrow_impl[A:Manifest](m: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[Unit]): Rep[Unit]
  def matrix_filterrows_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], pred: Rep[VectorView[A]] => Rep[Boolean])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  //def matrix_multiply_impl[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_times_vector_impl[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def matrix_sigmoid_impl[A:Manifest,MD:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[Double,MD], conv: Rep[A] => Rep[Double]): Rep[MD]
  def matrix_sigmoidf_impl[A:Manifest,MF:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[Float,MF], conv: Rep[A] => Rep[Double]): Rep[MF]
  def matrix_sumcol_impl[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def matrix_grouprowsby_impl[A:Manifest,K:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Rep[VectorView[A]] => Rep[K])(implicit b: MatrixBuilder[A,MA]): Rep[DenseVector[MA]] 
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiLACompiler with OptiLALift =>
  

  ///////////////
  // kernels

  // def matrix_apply_impl[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]) = {
  //   val offset = i*x.numCols+j
  //   dc_apply(x,offset)
  // }
  
  def matrix_getrow_impl[A:Manifest](m: Interface[Matrix[A]], row: Rep[Int]) = m.vview(row*m.numCols, 1, m.numCols, true)
  def matrix_getcol_impl[A:Manifest](m: Interface[Matrix[A]], col: Rep[Int]) = m.vview(col, m.numCols, m.numRows, false)
    
  def matrix_slice_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int])(implicit b: MatrixBuilder[A,MA]) = {
    //m.chkRange(beginrow, endrow)
    // Add check for col out of bounds
    // TODO: convert to view
    val resultOut = b.alloc(endRow-startRow, endCol-startCol)
    val result = b.toIntf(resultOut)   
    var i = startRow
    while (i < endRow) {
      var j = startCol
      while (j < endCol) {
        result(i-startRow, j-startCol) = m(i,j)
        j += 1
      }
      i += 1
    }
    resultOut.unsafeImmutable
  }

  def matrix_slicerows_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], start: Rep[Int], end: Rep[Int])(implicit b: MatrixBuilder[A,MA]) = {
    //m.chkRange(start, end)
    val resultOut = b.alloc(end-start, m.numCols)
    val result = b.toIntf(resultOut)     
    var i = start
    while (i < end) {
      var j = unit(0)
      while (j < m.numCols) {
        result(i-start, j) = m(i,j)
        j += 1
      }
      i += 1
    }
    resultOut.unsafeImmutable
  }

  def matrix_clone_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA]) = {
    val resultOut = b.alloc(m.numRows, m.numCols)
    val result = b.toIntf(resultOut) 
    var i = unit(0)    
    while (i < m.numRows) {
      var j = unit(0)
      while (j < m.numCols) {
        result(i,j) = m(i,j)
        j += 1
      }
      i += 1
    }
    resultOut.unsafeImmutable
  }
  
  def matrix_addrow_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: MatrixBuilder[A,MA]): Rep[MA] = {
    // val out = m.mutable()
    // out += y
    // out.unsafeImmutable
    val resultOut = b.alloc(m.numRows+1,m.numCols)
    val result = b.toIntf(resultOut)
    for (i <- 0 until m.size) {
      result.dcUpdate(i, m.dcApply(i))
    }
    for (j <- 0 until y.length) {
      result(m.numRows,j) = y(j)
    }
    resultOut.unsafeImmutable    
  }  
    
  def matrix_updaterow_impl[A:Manifest](m: Interface[Matrix[A]], row: Rep[Int], y: Interface[Vector[A]]) = {
    //chkEquals(x.length, numCols)
    // TODO: could be parallelized using a view
    var j = unit(0)
    while(j < m.numCols){
      m(row,j) = y(j)
      j += 1
    }
  }

  def matrix_equals_impl[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]]) = {
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

  def matrix_transpose_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA]) = {
    // naive, should block
    val resultOut = b.alloc(m.numCols, m.numRows)
    val result = b.toIntf(resultOut)
    for (i <- 0 until result.numRows){
      for (j <- 0 until result.numCols){
        result(i,j) = m(j,i)
      }
    }
    resultOut.unsafeImmutable
  }

  def matrix_pprint_impl[A:Manifest](m: Interface[Matrix[A]]) = {
    for (i <- 0 until m.numRows){
      print("[ ")
      for (j <- 0 until m.numCols){
        print(m(i,j))
        print(" ")
      }
      print("]\\n")
    }
  }

  def matrix_repmat_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], iRep: Rep[Int], jRep: Rep[Int])(implicit b: MatrixBuilder[A,MA]) = {
    val resultOut = b.alloc(iRep*m.numRows, jRep*m.numCols)
    val result = b.toIntf(resultOut)
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
            result(ii*m.numRows+i, jj*m.numCols+j) = m(i,j)
            //index += 1
            j += 1
          }
          jj += 1
        }
        i += 1
      }
      ii += 1
    }
    resultOut.unsafeImmutable
  }

  /*
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
  */
  
  def matrix_minrow_impl[A:Manifest:Ordering:HasMinMax,VA:Manifest](m: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA] = {
    throw new UnsupportedOperationException("not implemented yet")
//    val sumRows = m.mapRowsToVec[B](row => row.sum[B])
//    val idx = sumRows.minIndex
//    m(idx).clone
  }

  def matrix_maxrow_impl[A:Manifest:Ordering:HasMinMax,VA:Manifest](m: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA] = {
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

  def matrix_filterrows_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], pred: Rep[VectorView[A]] => Rep[Boolean])(implicit b: MatrixBuilder[A,MA]) = {
    val resultOut = b.alloc(0,0)
    val result = b.toIntf(resultOut)
    for (i <- 0 until m.numRows){
      val vv = m.getRow(i)
      if (pred(vv))
        result += vv.Clone // AKS TODO: should not need to clone
    }
    resultOut.unsafeImmutable
  }

  // def matrix_multiply_impl[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]] = {
  // 
  //   val yTrans = y.t
  //   val out = Matrix[A](x.numRows, y.numCols)
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

  def matrix_times_vector_impl[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA] = {
//  (0::x.numRows).t { rowIdx =>
//    x.getRow(rowIdx) *:* y
//  }

    val resultOut = b.alloc(x.numRows, false)
    val result = b.toIntf(resultOut)
    for (rowIdx <- 0 until x.numRows) {
      result(rowIdx) = x.getRow(rowIdx) *:* y
    }
    resultOut.unsafeImmutable
  }

  def matrix_sigmoid_impl[A:Manifest,MD:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[Double,MD], conv: Rep[A] => Rep[Double]): Rep[MD] = {
    val resultOut = b.alloc(x.numRows,x.numCols)
    val result = b.toIntf(resultOut)
    var i = 0
    while (i < x.numRows) {
      var j = 0
      while (j < x.numCols) {
        result(i,j) = (1.0/(1.0+exp(conv(x(i,j))*(-1))))
        j += 1
      }
      i += 1
    }
    resultOut
  }

  def matrix_sigmoidf_impl[A:Manifest,MF:Manifest](x: Interface[Matrix[A]])(implicit b:MatrixBuilder[Float,MF], conv: Rep[A] => Rep[Double]): Rep[MF] = {
    val resultOut = b.alloc(x.numRows,x.numCols)
    val result = b.toIntf(resultOut)
    var i = 0
    while (i < x.numRows) {
      var j = 0
      while (j < x.numCols) {
        result(i,j) = (1.0/(1.0+exp(conv(x(i,j))*(-1)))).AsInstanceOf[Float]
        j += 1
      }
      i += 1
    }
    resultOut
  }

  def matrix_sumcol_impl[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA] = {
    val resultOut = b.alloc(x.numCols,true)
    val result = b.toIntf(resultOut)
    for(colIdx <- 0 until x.numCols) {
      result(colIdx) = x.getCol(colIdx).sum
    }
    resultOut.unsafeImmutable
  }
  
  def matrix_grouprowsby_impl[A:Manifest,K:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Rep[VectorView[A]] => Rep[K])(implicit b: MatrixBuilder[A,MA]): Rep[DenseVector[MA]] = {
    val groups = HashMap[K,MA]()
    
    var i = 0
    while (i < x.numRows) {
      val key = pred(x(i))      
      if (!(groups contains key)) {
        groups(key) = b.alloc(0,x.numCols).unsafeImmutable
      }
      //b.toIntf(groups(key)) += x(i).Clone // AKS TODO: should not need clone
      groups(key) = (b.toIntf(groups(key)) :+ x(i)).ops.elem.asInstanceOf[Rep[MA]] // inefficient, but have to follow nested mutable rule      
      i += 1
    }
  
    val out = DenseVector[MA](0,true)
    for (m <- groups.values) {
      out += m.unsafeImmutable       
    }    
    out.unsafeImmutable
  }
    
}
