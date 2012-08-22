package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.delite.framework.datastructures.DeliteArray
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
  
  /*
   * Sparsematrix COO -> CSR conversion
   */
      
  def sparsematrix_coo_to_csr_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]]): Rep[SparseMatrix[A]] = {            
    if (sparsematrix_coo_ordered(m.nnz, sparsematrix_coo_raw_rowindices(m),sparsematrix_coo_raw_colindices(m)))
      sparsematrix_coo_to_csr_ordered(m)
    else 
      sparsematrix_coo_to_csr_unordered(m)
  }
  
  protected def sparsematrix_coo_ordered(nnz: Rep[Int], rowIndices: Rep[DeliteArray[Int]], colIndices: Rep[DeliteArray[Int]]) = {
    var i = 0
    var lastRow = 0
    var lastCol = 0
    var outOfOrder = false
    while (i < nnz && !outOfOrder) {
      if (rowIndices(i) < lastRow)
        outOfOrder = true
      if (rowIndices(i) == lastRow && colIndices(i) < lastCol)
        outOfOrder = true
      lastRow = rowIndices(i)
      lastCol = colIndices(i)
      i += 1
    }    
    !outOfOrder
  }
  
  protected def sparsematrix_coo_to_csr_ordered[A:Manifest](m: Rep[SparseMatrixBuildable[A]]): Rep[SparseMatrix[A]] = {        
    val data = sparsematrix_coo_raw_data(m)
    val rowIndices = sparsematrix_coo_raw_rowindices(m)
    val colIndices = sparsematrix_coo_raw_colindices(m)
    
    val dataOut = DeliteArray[A](m.nnz)
    val colIndicesOut = DeliteArray[Int](m.nnz)
    val rowPtrOut = DeliteArray[Int](m.numRows+1)    
    
    var i = 0
    while (i < m.nnz) {      
      darray_unsafe_update(colIndicesOut, i, colIndices(i))
      darray_unsafe_update(dataOut, i, data(i))      
      darray_unsafe_update(rowPtrOut, rowIndices(i)+1, rowPtrOut(rowIndices(i)+1)+1)
      i += 1
    }
    
    sparsematrix_coo_to_csr_finalize(m,dataOut,colIndicesOut,rowPtrOut)                
  }

  protected def sparsematrix_coo_to_csr_unordered[A:Manifest](m: Rep[SparseMatrixBuildable[A]]): Rep[SparseMatrix[A]] = {        
      val data = sparsematrix_coo_raw_data(m)
      val rowIndices = sparsematrix_coo_raw_rowindices(m)
      val colIndices = sparsematrix_coo_raw_colindices(m)
 
    // build a hashmap containing the elements of the COO matrix, 
    // with tuples mapped to longs and rowIndices in the high bits so we can sort by them. 
    // TODO: switch to using a specialized HashMap impl to avoid boxing
    val elems = HashMap[Long,A]()
    
    // remove duplicates by preferring elements further to the right in the array    
    var i = 0
    while (i < m.nnz) {
      if (rowIndices(i) >= 0) {  // removed elements are represented by a negative index
        val key = (rowIndices(i).toLong << 32) + colIndices(i).toLong
        elems(key) = data(i)
      }
      i += 1
    }
    val indices = elems.keys.toArray.sort // can we avoid the toArray?
    val dataOut = DeliteArray[A](indices.length)
    val colIndicesOut = DeliteArray[Int](indices.length)
    val rowPtrOut = DeliteArray[Int](m.numRows+1)    
    
    // write to output in sorted order without duplicates
    // left-to-right, top-to-bottom
    i = 0
    while (i < indices.length) {      
      val colIdx = (indices(i) & unit(0x00000000ffffffffL)).toInt
      darray_unsafe_update(colIndicesOut, i, colIdx)
      darray_unsafe_update(dataOut, i, elems(indices(i)))      
      
      val rowIdx = (indices(i) >>> 32).toInt
      darray_unsafe_update(rowPtrOut, rowIdx+1, rowPtrOut(rowIdx+1)+1)
      i += 1
    }
            
    sparsematrix_coo_to_csr_finalize(m,dataOut,colIndicesOut,rowPtrOut)
  }
  
  protected def sparsematrix_coo_to_csr_finalize[A:Manifest](m: Rep[SparseMatrixBuildable[A]], dataOut: Rep[DeliteArray[A]], colIndicesOut: Rep[DeliteArray[Int]], rowPtrOut: Rep[DeliteArray[Int]]) = {
    // finalize rowPtr    
    var i = 0
    var acc = 0
    while (i < m.numRows) {
      acc += rowPtrOut(i)
      darray_unsafe_update(rowPtrOut, i, acc)
      i += 1
    }
    darray_unsafe_update(rowPtrOut, m.numRows, dataOut.length)        
    
    // -- debug
    // println("indices.length: " + indices.length)
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
    sparsematrix_set_nnz(out, dataOut.length)    
    out.unsafeImmutable    
  }
  
  
  /**
   * remainder of SparseMatrixBuildable interface
   */
   
  protected def sparsematrix_coo_find_offset[A:Manifest](m: Rep[SparseMatrixBuildable[A]], row: Rep[Int], col: Rep[Int]): Rep[Int] = {
    val rowIndices = sparsematrix_coo_raw_rowindices(m)
    val colIndices = sparsematrix_coo_raw_colindices(m)
    
    // linear scan, prefer elements further to the right in the case of duplicates
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
     // another option: rename nnz to internalSize, and implement nnz to do a scan when called (has to deal with dupes)
    sparsematrix_buildable_append_impl(m,i,j,y,true)  
  }
  
  def sparsematrix_buildable_append_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int], y: Rep[A], alwaysWrite: Boolean = false): Rep[Unit] = {
    val shouldAppend = if (alwaysWrite) unit(true) else (y != defaultValue[A])  // conditional evaluated at staging time
    if (shouldAppend) {
      sparsematrix_coo_ensureextra(m, 1)    
      darray_unsafe_update(sparsematrix_coo_raw_data(m), m.nnz, y)
      darray_unsafe_update(sparsematrix_coo_raw_rowindices(m), m.nnz, i)
      darray_unsafe_update(sparsematrix_coo_raw_colindices(m), m.nnz, j)
      sparsematrix_buildable_set_nnz(m, m.nnz+1) 
    }
  }

  /* 
   * Note: we could have faster versions of the following methods if we specialize
   * them for inserting/removing sparse vectors/matrices explicitly.
   */
  
  def sparsematrix_buildable_insertrow_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit] = {
    if (m.size == 0) sparsematrix_buildable_set_numcols(m, y.length)
    val rowIndices = sparsematrix_coo_raw_rowindices(m)        
    for (i <- 0 until m.nnz) {
      if (rowIndices(i) >= pos)
        darray_unsafe_update(rowIndices, i, rowIndices(i)+1)
    }    
    for (j <- 0 until y.length) {
      sparsematrix_buildable_append_impl(m, pos, j, y(j))
    }            
    sparsematrix_buildable_set_numrows(m, m.numRows+1)    
  }

  def sparsematrix_buildable_insertallrows_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], xs: Interface[Matrix[A]]): Rep[Unit] = {
    if (m.size == 0) sparsematrix_buildable_set_numcols(m, xs.numCols)
    val rowIndices = sparsematrix_coo_raw_rowindices(m)    
    for (i <- 0 until m.nnz) {
      if (rowIndices(i) >= pos)
        darray_unsafe_update(rowIndices, i, rowIndices(i)+xs.numRows)
    }    
    for (i <- 0 until xs.numRows) {
      for (j <- 0 until xs.numCols) {
        sparsematrix_buildable_append_impl(m, pos+i, j, xs(i,j))
      }
    }                
    sparsematrix_buildable_set_numrows(m, m.numRows+xs.numRows)
  }

  def sparsematrix_buildable_insertcol_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit] = {    
    if (m.size == 0) sparsematrix_buildable_set_numrows(m, y.length)
    val colIndices = sparsematrix_coo_raw_colindices(m)    
    for (i <- 0 until m.nnz) {
      if (colIndices(i) >= pos)
        darray_unsafe_update(colIndices, i, colIndices(i)+1)
    }    
    for (i <- 0 until y.length) {
      sparsematrix_buildable_append_impl(m, i, pos, y(i))
    }    
    sparsematrix_buildable_set_numcols(m, m.numCols+1)
  }

  def sparsematrix_buildable_insertallcols_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], xs: Interface[Matrix[A]]): Rep[Unit] = {
    if (m.size == 0) sparsematrix_buildable_set_numrows(m, xs.numRows)
    val colIndices = sparsematrix_coo_raw_colindices(m)    
    for (i <- 0 until m.nnz) {
      if (colIndices(i) >= pos)
        darray_unsafe_update(colIndices, i, colIndices(i)+xs.numCols)
    }    
    for (i <- 0 until xs.numRows) {
      for (j <- 0 until xs.numCols) {
        sparsematrix_buildable_append_impl(m, i, pos+j, xs(i,j))
      }
    }                
    sparsematrix_buildable_set_numcols(m, m.numCols+xs.numCols)    
  }

  def sparsematrix_buildable_removerows_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], num: Rep[Int]): Rep[Unit] = {
    // FIXME: this will also leave nnz in a bad state
    val rowIndices = sparsematrix_coo_raw_rowindices(m)    
    for (i <- 0 until m.nnz) {
      if (rowIndices(i) >= pos && rowIndices(i) < pos+num) {  
        // remove by setting it to invalid
        darray_unsafe_update(rowIndices, i, -1) 
        darray_unsafe_update(sparsematrix_coo_raw_colindices(m), i, -1) 
        // darray_unsafe_update(sparsematrix_coo_raw_data(m), i, defaultValue[A]) 
      }
      else if (rowIndices(i) >= pos+num)
        darray_unsafe_update(rowIndices, i, rowIndices(i)-num)
    }        
    sparsematrix_buildable_set_numrows(m, m.numRows-num)
  }

  def sparsematrix_buildable_removecols_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], num: Rep[Int]): Rep[Unit] = {
    // FIXME: this will also leave nnz in a bad state
    val colIndices = sparsematrix_coo_raw_colindices(m)    
    for (i <- 0 until m.nnz) {
      if (colIndices(i) >= pos && colIndices(i) < pos+num) {
        // remove by setting it to invalid
        darray_unsafe_update(colIndices, i, -1) 
        darray_unsafe_update(sparsematrix_coo_raw_rowindices(m), i, -1) 
        // darray_unsafe_update(sparsematrix_coo_raw_data(m), i, defaultValue[A]) 
      }
      else if (colIndices(i) >= pos+num)
        darray_unsafe_update(colIndices, i, colIndices(i)-num)
    }        
    sparsematrix_buildable_set_numcols(m, m.numCols-num)    
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
