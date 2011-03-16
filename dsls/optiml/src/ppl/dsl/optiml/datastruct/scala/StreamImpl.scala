package ppl.dsl.optiml.datastruct.scala

class StreamImpl[T:Manifest](val numRows: Int, val numCols: Int, val chunkSize: Int, val func: (Int,Int) => T, val isPure: Boolean) extends Stream[T] {
//  var currentChunk = 0
  protected var _data: Array[T] = new Array[T](size)

  def bufRows = math.min(numRows, chunkSize)
  def size = numCols*bufRows
  def data = _data

  if (size < 0) throw new RuntimeException("Stream overflowed during initialization")

  def rowsIn(offset: Int) = {
    val remainingRows = numRows - offset*chunkSize
    val leftover = if (remainingRows < 0) numRows else remainingRows // in case numRows < chunkSize
    math.min(chunkSize, leftover).asInstanceOf[Int]
  }

  def initChunk(offset: Int) {
    for (i <- 0 until rowsIn(offset)) {
      initRow(i, offset)
    }
//    currentChunk = offset
  }

  def initRow(row: Int, offset: Int) {
    for (j <- 0 until numCols) {
      _data(row*numCols+j) = func(offset*chunkSize+row,j)
    }
  }

  // chunk management must be done inside the op (foreachRows), not in the impl
  // here, we just have to assume that init has been called appropriately and idx points to the right chunk

  def chunkRow(idx: Int, offset: Int): VectorView[T] = {
    //vview(idx*numCols, 1, numCols, true)
    new StreamRowImpl[T](idx, offset, this, _data)
  }

  def chunkElem(idx: Int, j: Int): T = {
    _data(idx*numCols+j)
  }

  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T] = {
    new VectorViewImpl[T](_data, start, stride, length, isRow)
  }

}