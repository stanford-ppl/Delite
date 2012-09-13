package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class GrayscaleImage(nRows: Int, nCols: Int) {   
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[Double] = new Array[Double](nRows*nCols)  
}