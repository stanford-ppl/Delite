package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila._

 /*
  * This implements the SparseMatrixBuildable interface with a COO Matrix. Other options can be used by mixing their
  * implementation (and a corresponding compatible SparseMatrix implementation) in instead of this one.
  * 
  * We represent the COO matrix as three arrays (data, row, col) where data(i) represents a non-zero element at (row(i),col(i)).
  * The arrays are kept unsorted and duplicates are allowed; inserts and updates are fast (O(1)), while apply is O(N).
  */
trait SparseMatrixCOOImplOps extends SparseMatrixBuildableImplOps {
  this: OptiLACompiler with SparseMatrixCOOCompilerOps with OptiLALift =>
  
  ///////////////
  // kernels
  
  def sparsematrix_coo_to_csr_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]]): Rep[SparseMatrix[A]] = {        
    val data = sparsematrix_coo_raw_data(m)
    val rowIndices = sparsematrix_coo_raw_rowindices(m)
    val colIndices = sparsematrix_coo_raw_colindices(m)
    
    // build a hashmap containing the elements of the COO matrix, 
    // with tuples mapped to longs and rowIndices in the high bits so we can sort by them. 
    val elems = HashMap[Long,A]()
    
    // remove duplicates by preferring elements further to the right in the array    
    // can we do a faster scan to tell us if we can skip this step (i.e. there are no dups)?
    var i = 0
    while (i < m.nnz) {
      val key = (rowIndices(i).toLong << 32) + colIndices(i).toLong
      elems(key) = data(i)
      i += 1
    }

    val indices = elems.keys.toArray.sort // can we avoid the toArray?
    val dataOut = DeliteArray[A](indices.length)
    val colIndicesOut = DeliteArray[Int](indices.length)
    
    // write to output in sorted order without duplicates
    // left-to-right, top-to-bottom
    i = 0
    while (i < indices.length) {      
      val colIdx = (indices(i) & unit(0x00000000ffffffffL)).toInt
      darray_unsafe_update(colIndicesOut, i, colIdx)
      darray_unsafe_update(dataOut, i, elems(indices(i)))      
      i += 1
    }
            
    // update rowPtr    
    val rowPtrOut = DeliteArray[Int](m.numRows+1)    
    darray_unsafe_update(rowPtrOut, m.numRows, m.nnz)    
    i = 0
    while (i < m.nnz) {
      val idx = rowIndices(i)+1
      darray_unsafe_update(rowPtrOut, idx, rowPtrOut(idx)+1)
      i += 1
    }
    i = 0
    var acc = 0
    while (i < m.numRows) {
      acc += rowPtrOut(i)
      darray_unsafe_update(rowPtrOut, i, acc)
      i += 1
    }
    
    // -- debug
    // println("input to coo->csr: ")
    // println("data: " + data)
    // println("colIndices: " + colIndices)
    // println("rowIndices: " + rowIndices)
    // 
    // println("output of coo->csr: ")
    // println("data: " + dataOut)
    // println("colIndices: " + colIndicesOut)
    // println("rowPtr: " + rowPtrOut)
    
    val out = sparsematrix_csr_new[A](m.numRows,m.numCols)        
    sparsematrix_csr_set_raw_data(out, dataOut.unsafeImmutable)      
    sparsematrix_csr_set_raw_colindices(out, colIndicesOut.unsafeImmutable)
    sparsematrix_csr_set_raw_rowptr(out, rowPtrOut.unsafeImmutable)
    sparsematrix_set_nnz(out, indices.length)    
    out.unsafeImmutable
  }
  
  protected def sparsematrix_coo_find_offset[A:Manifest](m: Rep[SparseMatrixBuildable[A]], row: Rep[Int], col: Rep[Int]): Rep[Int] = {
    val rowIndices = sparsematrix_coo_raw_rowindices(m)
    val colIndices = sparsematrix_coo_raw_colindices(m)
    
    // linear scan
    var i = 0
    var foundAtIndex = -1
    while (i < m.nnz) {
      if (rowIndices(i) == row && colIndices(i) == col)
        foundAtIndex = i
      i += 1
    }
    
    foundAtIndex
  }
    
  def sparsematrix_buildable_apply_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int]): Rep[A] = {    
    warn("possible performance problem - reading from a sparse matrix COO representation")
    
    val data = sparsematrix_coo_raw_data(m)
    val offRaw = sparsematrix_coo_find_offset(m,i,j)
    if (offRaw > -1) data(offRaw)
    else defaultValue[A]
  }
  
  def sparsematrix_buildable_update_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit] = {    
    // duplicates are allowed, so don't bother checking if the value already exists
    // the last value in the array (furthest to the right is the true value)
    
     // FIXME: if this updated an existing value, nnz is wrong...
     // without scanning, we won't know whether or not nnz should be incremented
     // one (perhaps bad) option: don't expose nnz in SparseMatrixBuildable interface
    sparsematrix_buildable_append_impl(m,i,j,y)  
  }
  
  def sparsematrix_buildable_append_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit] = {
    val data = sparsematrix_coo_raw_data(m)
    val rowIndices = sparsematrix_coo_raw_rowindices(m)
    val colIndices = sparsematrix_coo_raw_colindices(m)
    sparsematrix_coo_ensureextra(m, 1)    
    darray_unsafe_update(data, m.nnz, y)
    darray_unsafe_update(rowIndices, m.nnz, i)
    darray_unsafe_update(colIndices, m.nnz, j)
    sparsematrix_buildable_set_nnz(m, m.nnz+1) 
  }

  def sparsematrix_buildable_insertrow_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit] = {
    throw new UnsupportedOperationException("tbd")
  }

  def sparsematrix_buildable_insertallrows_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], xs: Interface[Matrix[A]]): Rep[Unit] = {
    throw new UnsupportedOperationException("tbd")
  }

  def sparsematrix_buildable_insertcol_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit] = {
    throw new UnsupportedOperationException("tbd")
  }

  def sparsematrix_buildable_insertallcols_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], xs: Interface[Matrix[A]]): Rep[Unit] = {
    throw new UnsupportedOperationException("tbd")
  }

  def sparsematrix_buildable_removerows_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], num: Rep[Int]): Rep[Unit] = {
    throw new UnsupportedOperationException("tbd")
  }

  def sparsematrix_buildable_removecols_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], num: Rep[Int]): Rep[Unit] = {
    throw new UnsupportedOperationException("tbd")
  }

  protected def sparsematrix_coo_ensureextra[A:Manifest](m: Rep[SparseMatrixBuildable[A]], extra: Rep[Int]): Rep[Unit] = {
    if (sparsematrix_coo_raw_data(m).length - m.nnz < extra) {
      sparsematrix_coo_realloc(m,m.nnz + extra)
    }    
  }

  protected def sparsematrix_coo_realloc[A:Manifest](m: Rep[SparseMatrixBuildable[A]], minLen: Rep[Int]): Rep[Unit] = {
    val data = sparsematrix_coo_raw_data(m)
    val rowIndices = sparsematrix_coo_raw_rowindices(m)
    val colIndices = sparsematrix_coo_raw_colindices(m)
    var n = max(4, data.length * 2)
    while (n < minLen) n = n*2
    val dataOut = DeliteArray[A](n)
    val rowIndicesOut = DeliteArray[Int](n)
    val colIndicesOut = DeliteArray[Int](n)
    darray_unsafe_copy(data, 0, dataOut, 0, m.nnz)
    darray_unsafe_copy(rowIndices, 0, rowIndicesOut, 0, m.nnz)
    darray_unsafe_copy(colIndices, 0, colIndicesOut, 0, m.nnz)
    sparsematrix_coo_set_raw_data(m, dataOut.unsafeImmutable)
    sparsematrix_coo_set_raw_rowindices(m, rowIndicesOut.unsafeImmutable)
    sparsematrix_coo_set_raw_colindices(m, colIndicesOut.unsafeImmutable)    
  }
}
