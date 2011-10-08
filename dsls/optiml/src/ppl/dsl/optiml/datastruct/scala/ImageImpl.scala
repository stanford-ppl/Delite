package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class ImageImpl[T:Manifest](nRows: Int, nCols: Int) extends MatrixImpl[T](nRows, nCols) with Image[T] {
  def this(xs: Matrix[T]){
    this(0,0)
    _data = xs.data
    _numRows = xs.numRows
    _numCols = xs.numCols
  }
}