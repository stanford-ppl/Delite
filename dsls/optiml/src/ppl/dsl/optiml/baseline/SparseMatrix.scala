package ppl.dsl.optiml.baseline

object SparseMatrix {
  def apply[A:Numeric:Manifest](numRows: Int, numCols: Int) = new SparseMatrix[A](numRows,numCols)
}

class SparseMatrix[@specialized T:Numeric:Manifest](__numRows: Int, __numCols: Int) extends Matrix[T] with SparseUtil {
  def numRows: Int = _numRows
  def numCols: Int = _numCols
  
  var _numRows = __numRows
  var _numCols = __numCols  
  var _data = new Array[T](32)
  // column index of each non-zero value
  var _colIndices = new Array[Int](32)
  // starting location of each row, e.g. (_rowPtr(3) = 5 means that row 3 starts at value index 5)
  var _rowPtr = new Array[Int](_numRows+1)
  var _nnz = 0
    
  def apply(i: Int, j: Int): T  = {
    val offRaw = bsearch(_colIndices, _rowPtr(i), _rowPtr(i+1)-1, j)
    if (offRaw > -1) _data(offRaw) else implicitly[Numeric[T]].zero
  }

  def newVector[B:Numeric:Manifest](len: Int): Vector[B] = SparseVector[B](len)
  def vview(start: Int, stride: Int, length: Int): Vector[T] = new SparseVectorView[T](this, start, stride, length)
  
  def update(i: Int, j: Int, y: T): Unit = {
    val offRaw = bsearch(_colIndices, _rowPtr(i), _rowPtr(i+1)-1, j)
    if (offRaw > -1) _data(offRaw) = y
    else {
      if (y != implicitly[Numeric[T]].zero) {
        val off = ~offRaw
        insertSpace(off, 1)
        _colIndices(off) = j
        _data(off) = y
        // have to increment every element of rowPtr
        for (row <- i+1 until _rowPtr.length) {
          _rowPtr(row) += 1
        }
      }
    }        
  }

  protected def ensureExtra(extra: Int): Unit = {
    if (_data.length - _nnz < extra) {
      realloc(_nnz + extra)
    }
  }
  
  protected def realloc(minLen: Int): Unit = {
    var n = java.lang.Math.max(4, _data.length * 2)
    while (n < minLen) n = n*2
    val dataOut = new Array[T](n)
    val colIndicesOut = new Array[Int](n)
    System.arraycopy(_data, 0, dataOut, 0, _nnz)
    System.arraycopy(_colIndices, 0, colIndicesOut, 0, _nnz)
    _data = dataOut
    _colIndices = colIndicesOut
  }
  
  protected def insertSpace(pos: Int, len: Int): Unit = {
    ensureExtra(len)
    System.arraycopy(_data, pos, _data, pos + len, _nnz - pos)
    System.arraycopy(_colIndices, pos, _colIndices, pos + len, _nnz - pos)
    _nnz += len
  }
  
}