package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
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
    warn("possible performance problem - writing to a sparse matrix CSR representation")    
    
    val offRaw = sparsematrix_csr_find_offset(m,i,j)    
    if (offRaw > -1) darray_unsafe_update(sparsematrix_csr_raw_data(m), offRaw, y)
    else {
      // TODO AKS: test this
      if (y != defaultValue[A]) {
        val off = ~offRaw
        sparsematrix_csr_insertspace(m, off, 1)
        darray_unsafe_update(sparsematrix_csr_raw_colindices(m), off, j)
        darray_unsafe_update(sparsematrix_csr_raw_data(m), off, y)     
        val rowPtr = sparsematrix_csr_raw_rowptr(m)
        // have to increment every element of rowPtr
        for (row <- i until rowPtr.length) {
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