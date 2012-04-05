package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class Image[T:Manifest](nRows: Int, nCols: Int) {   
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[T] = new Array[T](nRows*nCols)  
}