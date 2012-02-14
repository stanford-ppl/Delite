package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class Stream[T:Manifest](val numRows: Int, val numCols: Int, val chunkSize: Int, val func: (Int,Int) => T, val isPure: Boolean) { //extends Stream[T] {
    val bufRows = math.min(numRows, chunkSize)
    val size = numCols*bufRows
    protected var _data: Array[T] =  try { new Array[T](size) }
                                     catch {
                                       case _ => throw new RuntimeException("Stream overflowed during initialization")
                                     }
    def data = _data

    def initRow(row: Int, offset: Int) {
      var j = 0
      val lhsOff = row*numCols
      val rhsOff = offset*chunkSize+row
      while (j < numCols) {
        _data(lhsOff+j) = func(rhsOff,j)
        j += 1
      }
    }

    // chunk management must be done inside the op (foreachRows), not in the impl
    // here, we just have to assume that init has been called appropriately and idx points to the right chunk

    def chunkRow(idx: Int, offset: Int) = {
      //vview(idx*numCols, 1, numCols, true)
      new StreamRow[T](idx, offset, this, _data)
    }

    def rawElem(idx: Int): T = {
      _data(idx)
    }

    def vview(start: Int, stride: Int, length: Int, isRow: Boolean) = {
      new VectorView[T](_data, start, stride, length, isRow)
    }
}