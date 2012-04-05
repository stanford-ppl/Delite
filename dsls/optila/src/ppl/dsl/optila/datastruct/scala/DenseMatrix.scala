package ppl.dsl.optila.datastruct.scala

class DenseMatrix[@specialized T: Manifest](nRows: Int, nCols: Int) { 
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[T] = new Array[T](nRows*nCols)
    
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */
  def unsafeSetData(xs: Array[T], len: Int) {
    _data = xs
    // _length = len
  }
  
  def Clone = {    
    val res = new DenseMatrix[T](_numRows, _numCols)
    res._data = _data.clone
    res
  }  
}

