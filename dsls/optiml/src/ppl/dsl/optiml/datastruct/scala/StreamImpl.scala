package ppl.dsl.optiml.datastruct.scala

class StreamImpl[T:ClassManifest](nRows: Int, nCols: Int, func: (Int,Int) => T) extends Stream[T] {
  var currentChunk = 0

  protected var _numRows = nRows
  protected var _numCols = nCols
  protected var _data: Array[T] = new Array[T](size)

  def numRows = _numRows
  def numCols = _numCols
  def size = _numRows*_numCols
  def data = _data

  def init(offset: Int, chunkSize: Int) {
    for (i <- 0 until chunkSize) {
      for (j <- 0 until nCols) {
        _data(i*numCols+j) = func(offset+i,j)
      }
    }
    currentChunk = offset
  }

  // chunk management must be done inside the op (foreachRows), not in the impl
  // here, we just have to assume that init has been called appropriately and idx points to the right chunk

  def chunkRow(idx: Int): VectorView[T] = {
    vview(idx*numCols, 1, numCols, true)
  }

  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T] = {
    new VectorViewImpl[T](_data, start, stride, length, isRow)
  }

}