package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.delite.framework.datastructures.DeliteArray
import ppl.dsl.optila._

/**
 * This file implements the Matrix interface for a CSR SparseMatrix.
 */ 
trait SparseMatrixCSRImplOps extends SparseMatrixImplOps {
  this: OptiLACompiler with SparseMatrixCSRCompilerOps with OptiLALift =>
  

  ///////////////
  // kernels
  
  def sparsematrix_vview_impl[A:Manifest](x: Rep[SparseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[SparseVectorView[A]] = {
    SparseVectorView[A](x, start, stride, length, isRow)
  }
  
  protected def sparsematrix_csr_find_offset[A:Manifest](m: Rep[SparseMatrix[A]], row: Rep[Int], col: Rep[Int]): Rep[Int] = {
    val rowPtr = sparsematrix_csr_raw_rowptr(m)
    val colIndices = sparsematrix_csr_raw_colindices(m)
    // colIndices will be sorted between [rowPtr(row), rowPtr(row+1))
    bsearch(colIndices, rowPtr(row), rowPtr(row+1)-1, col)
  }
  
  def sparsematrix_apply_impl[A:Manifest](m: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A] = {    
    val data = sparsematrix_csr_raw_data(m)
    val offRaw = sparsematrix_csr_find_offset(m, i, j)
    if (offRaw > -1) data(offRaw) else defaultValue[A]    
  }
  
  def sparsematrix_update_impl[A:Manifest](m: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit] = {
    warn("performance: writing to a sparse matrix CSR representation")    
    
    val offRaw = sparsematrix_csr_find_offset(m,i,j)    
    if (offRaw > -1) darray_unsafe_update(sparsematrix_csr_raw_data(m), offRaw, y)
    else {
      if (y != defaultValue[A]) {
        val off = ~offRaw
        sparsematrix_csr_insertspace(m, off, 1)
        darray_unsafe_update(sparsematrix_csr_raw_colindices(m), off, j)
        darray_unsafe_update(sparsematrix_csr_raw_data(m), off, y)     
        val rowPtr = sparsematrix_csr_raw_rowptr(m)
        // have to increment every element of rowPtr
        for (row <- i+1 until rowPtr.length) {
          darray_unsafe_update(rowPtr,row,rowPtr(row)+1)
        }
      }
    }        
  }

  protected def sparsematrix_csr_ensureextra[A:Manifest](m: Rep[SparseMatrix[A]], extra: Rep[Int]): Rep[Unit] = {
    if (sparsematrix_csr_raw_data(m).length - m.nnz < extra) {
      sparsematrix_csr_realloc(m,m.nnz + extra)
    }
  }
  
  protected def sparsematrix_csr_realloc[A:Manifest](m: Rep[SparseMatrix[A]], minLen: Rep[Int]): Rep[Unit] = {
    val data = sparsematrix_csr_raw_data(m)
    val colIndices = sparsematrix_csr_raw_colindices(m)
    var n = max(4, data.length * 2)
    while (n < minLen) n = n*2
    val dataOut = DeliteArray[A](n)
    val colIndicesOut = DeliteArray[Int](n)
    darray_unsafe_copy(data, 0, dataOut, 0, m.nnz)
    darray_unsafe_copy(colIndices, 0, colIndicesOut, 0, m.nnz)
    sparsematrix_csr_set_raw_data(m, dataOut.unsafeImmutable)
    sparsematrix_csr_set_raw_colindices(m, colIndicesOut.unsafeImmutable)
  }
  
  protected def sparsematrix_csr_insertspace[A:Manifest](m: Rep[SparseMatrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    sparsematrix_csr_ensureextra(m,len)
    val data = sparsematrix_csr_raw_data(m)
    val colIndices = sparsematrix_csr_raw_colindices(m)
    darray_unsafe_copy(data, pos, data, pos + len, m.nnz - pos)
    darray_unsafe_copy(colIndices, pos, colIndices, pos + len, m.nnz - pos)
    sparsematrix_set_nnz(m, m.nnz + len)
  }

  def sparsematrix_csr_rowindices_impl(rowPtr: Rep[DeliteArray[Int]]) = {
    // for each data index, compute the row associated with it
  
    // ex.: 8 rows
    // rowPtr: 0 0 0 1 1 5 5 10
    // colIndex 0 is row 2
    //        1-4 is row 4
    //        5-9 is row 6  
    //         10 is row 7
    
    val nnz = rowPtr(rowPtr.length-1)
    val rows = DeliteArray[Int](nnz)      
    var i = 0
    var oldRow = rowPtr(0)
    while (i < rowPtr.length) {
      val nextRow = rowPtr(i)
      if (nextRow != oldRow) {
        for (j <- oldRow until nextRow) {
          rows(j) = i-1
        }
      }
      oldRow = nextRow
      i += 1
    }
    rows
  }    
  
  def sparsematrix_csr_zip_nz_union_impl[A:Manifest,B:Manifest,R:Manifest](ma: Rep[SparseMatrix[A]], mb: Rep[SparseMatrix[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[SparseMatrix[R]] = {
    val dataA = sparsematrix_csr_raw_data(ma)
    val colIndicesA = sparsematrix_csr_raw_colindices(ma)
    val rowPtrA = sparsematrix_csr_raw_rowptr(ma)
    val dataB = sparsematrix_csr_raw_data(mb)
    val colIndicesB = sparsematrix_csr_raw_colindices(mb)
    val rowPtrB = sparsematrix_csr_raw_rowptr(mb)         
    
    val outRowPtr = DeliteArray[Int](ma.numRows+1)
    val outColIndices = DeliteArray[Int](ma.nnz + mb.nnz) // upper bound
    val outData = DeliteArray[R](ma.nnz + mb.nnz)
    
    // TODO: how can we parallelize this efficiently?
    // -- sequential version, build the output in one loop
    var oldRowA = rowPtrA(0)
    var oldRowB = rowPtrB(0)
    var nnz = 0
    var i = 0
    
    while (i < ma.numRows+1) {
      // union of colIndicesA and colIndicesB at row i
      if (rowPtrA(i) != oldRowA || rowPtrB(i) != oldRowB) {
        // add to output from either A or B, maintaining sorted order
        var aIdx = oldRowA
        var bIdx = oldRowB
        while (aIdx < rowPtrA(i) || bIdx < rowPtrB(i)) {
          if (aIdx < rowPtrA(i) && bIdx < rowPtrB(i)) {
            // both A and B defined at this part of the row
            if (colIndicesA(aIdx) < colIndicesB(bIdx)) {                
              outColIndices(nnz) = colIndicesA(aIdx)
              outData(nnz) = f(dataA(aIdx),defaultValue[B])
              aIdx += 1
            } 
            else if (colIndicesA(aIdx) > colIndicesB(bIdx)) {
              outColIndices(nnz) = colIndicesB(bIdx)
              outData(nnz) = f(defaultValue[A],dataB(bIdx))
              bIdx += 1                
            }
            else {
              outColIndices(nnz) = colIndicesA(aIdx) // doesn't matter
              outData(nnz) = f(dataA(aIdx),dataB(bIdx))
              aIdx += 1
              bIdx += 1                                
            }
          }
          else if (aIdx < rowPtrA(i)) {
            // only A defined at this part of the row
            outColIndices(nnz) = colIndicesA(aIdx)
            outData(nnz) = f(dataA(aIdx), defaultValue[B])  
            aIdx += 1                          
          }
          else if (bIdx < rowPtrB(i)) {
            // only B defined at this part of the row
            outColIndices(nnz) = colIndicesB(bIdx)
            outData(nnz) = f(defaultValue[A], dataB(bIdx))                            
            bIdx += 1
          }
          else {
            fatal("sparsematrix_csr_zip_nz_union_impl should never reach here")
          }
          nnz += 1
        }       
      }
      outRowPtr(i) = nnz                
      oldRowA = rowPtrA(i)
      oldRowB = rowPtrB(i)
      i += 1
    }
    
    val out = sparsematrix_csr_new[R](ma.numRows, ma.numCols)
    sparsematrix_csr_set_raw_colindices(out, outColIndices.unsafeImmutable)
    sparsematrix_csr_set_raw_data(out, outData.unsafeImmutable)
    sparsematrix_csr_set_raw_rowptr(out, outRowPtr.unsafeImmutable)
    sparsematrix_set_nnz(out, outRowPtr(ma.numRows))
    out.unsafeImmutable
  }
  
  def sparsematrix_csr_zip_nz_intersection_impl[A:Manifest,B:Manifest,R:Manifest](ma: Rep[SparseMatrix[A]], mb: Rep[SparseMatrix[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[SparseMatrix[R]] = {
    val dataA = sparsematrix_csr_raw_data(ma)
    val colIndicesA = sparsematrix_csr_raw_colindices(ma)
    val rowPtrA = sparsematrix_csr_raw_rowptr(ma)
    val dataB = sparsematrix_csr_raw_data(mb)
    val colIndicesB = sparsematrix_csr_raw_colindices(mb)
    val rowPtrB = sparsematrix_csr_raw_rowptr(mb)         
    
    val outRowPtr = DeliteArray[Int](ma.numRows+1)
    val outColIndices = DeliteArray[Int](min(ma.nnz, mb.nnz)) 
    val outData = DeliteArray[R](min(ma.nnz,mb.nnz))
    
    // TODO: how can we parallelize this efficiently?
    // -- sequential version, build the output in one loop
    var oldRowA = rowPtrA(0)
    var oldRowB = rowPtrB(0)
    var nnz = 0
    var i = 0
    
    while (i < ma.numRows+1) {
      // intersection of colIndicesA and colIndicesB at row i
      if (rowPtrA(i) != oldRowA && rowPtrB(i) != oldRowB) {
        // add to output from A and B, maintaining sorted order
        var aIdx = oldRowA
        var bIdx = oldRowB
        while (aIdx < rowPtrA(i) && bIdx < rowPtrB(i)) {
          // both A and B defined at this part of the row
          if (colIndicesA(aIdx) < colIndicesB(bIdx)) {                
            aIdx += 1
          } 
          else if (colIndicesA(aIdx) > colIndicesB(bIdx)) {
            bIdx += 1                
          }
          else {
            outColIndices(nnz) = colIndicesA(aIdx) // doesn't matter
            outData(nnz) = f(dataA(aIdx),dataB(bIdx))
            aIdx += 1
            bIdx += 1                                
            nnz += 1
          }
        }
      }
      outRowPtr(i) = nnz                
      oldRowA = rowPtrA(i)
      oldRowB = rowPtrB(i)
      i += 1
    }
    
    val out = sparsematrix_csr_new[R](ma.numRows, ma.numCols)
    sparsematrix_csr_set_raw_colindices(out, outColIndices.unsafeImmutable)
    sparsematrix_csr_set_raw_data(out, outData.unsafeImmutable)
    sparsematrix_csr_set_raw_rowptr(out, outRowPtr.unsafeImmutable)
    sparsematrix_set_nnz(out, outRowPtr(ma.numRows))
    out.unsafeImmutable
  }
  
  
  /*  
  // TODO: try/catch, case, in embedded implementation? we need to lift these still.
  def sparsematrix_inverse_impl[A:Manifest](m: Rep[SparseMatrix[A]])(implicit conv: Rep[A] => Rep[Double]): Rep[SparseMatrix[Double]] = {
    //m.chkEquals(m.numCols, m.numRows)

    // augment the SparseMatrix with the identity SparseMatrix of the same size
    val id = SparseMatrix.identity(m.numCols)
    val augMat = m.toDouble.mutable
    augMat.insertAllCols(augMat.numCols, id)
//    try{
      // perform row reductions
      val redMat = sparsematrix_rreduce(augMat)
      // strip off the augmented SparseMatrix
      redMat.removeCols(0, m.numCols)
      redMat.unsafeImmutable
//    }
//    catch{
//      case e: Exception => {
//        println("Error: SparseMatrix appears to be singular")
//        throw new IllegalArgumentException
//      }
//    }
  }

   protected def sparsematrix_rreduce(m: Rep[SparseMatrix[Double]]): Rep[SparseMatrix[Double]] = {
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
}