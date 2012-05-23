package ppl.dsl.optila.datastruct.scala

/**
 * A sparse matrix in Compressed Sparse Row (CSR) format, efficient for reads and matrix arithmetic operations.
 * SparseMatrixCOO should be used to construct sparse matrices.
 * 
 * Should we use CSC? At the very least, we should probably have an alternative CSC implementation to experiment with.
 * In ML, m >> n, so CSC would require less storage. However, row slicing (to get a single training example) would be less efficient.
 * Since this is OptiLA, we should probably provide both.
 */
class SparseMatrixCSR[@specialized T: Manifest](__numRows: Int, __numCols: Int) { 
  var _numRows = __numRows
  var _numCols = __numCols
  // non-zero values, left-to-right then top-to-bottom
  var _data = new Array[T](32)
  // column index of each non-zero value
  var _colIndices = new Array[Int](32)
  // starting location of each row, e.g. (_rowPtr(3) = 5 means that row 3 starts at value index 5)
  var _rowPtr = new Array[Int](_numRows+1)
  var _nnz = 0

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException("unsafeSetData in SparseMatrixCSR not supported")  
  
  def Clone = { 
    val m = new SparseMatrixCSR[T](_numRows, _numCols)
    m._data = _data.clone
    m._colIndices = _colIndices.clone
    m._rowPtr = _rowPtr.clone
    m._nnz = _nnz
    m
  }  
}
