package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class GrayscaleImageImpl(nRows: Int, nCols: Int) extends MatrixImpl[Int](nRows, nCols) with GrayscaleImage {
  def this(xs: Matrix[Int]){
    this(0,0)
    _data = xs.data
    _numRows = xs.numRows
    _numCols = xs.numCols
  }
}