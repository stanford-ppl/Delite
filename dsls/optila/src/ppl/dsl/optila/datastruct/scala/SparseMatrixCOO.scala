package ppl.dsl.optila.datastruct.scala

/*
 * A sparse matrix in COOrdinate format. Efficient for constructing sparse matrices
 * and converting to other (CSR, CSC) formats.
 */ 
class SparseMatrixCOO[@specialized T: Manifest](__numRows: Int, __numCols: Int) { 
  var _numRows = __numRows
  var _numCols = __numCols
  // non-zero values, left-to-right then top-to-bottom
  var _data = new Array[T](32)
  // column index of each non-zero value
  var _colIndices = new Array[Int](32)
  // row index of each non-zero value
  var _rowIndices = new Array[Int](32)
  // number of non-zeros stored in the COO matrix
  var _nnz = 0

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException("unsafeSetData in SparseMatrixCOO not supported")  
  
  def Clone = { 
    val m = new SparseMatrixCOO[T](_numRows, _numCols)
    m._data = _data.clone
    m._colIndices = _colIndices.clone
    m._rowIndices = _rowIndices.clone
    m._nnz = _nnz
    m
  }  
}
