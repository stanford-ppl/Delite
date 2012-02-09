package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class Image[T:Manifest](nRows: Int, nCols: Int) { //extends DenseMatrix[T](nRows, nCols) with Image[T] {
  // def this(xs: DenseMatrix[T]){
  //   this(0,0)
  //   _data = xs.data
  //   _numRows = xs.numRows
  //   _numCols = xs.numCols
  // }
  
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[T] = new Array[T](nRows*nCols)  
}