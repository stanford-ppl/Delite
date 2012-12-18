package ppl.apps.interop

import scala.collection.parallel.mutable.ParArray
import Numeric.Implicits._

object ParMatrix {

  def apply[T:Numeric:Fractional:Manifest](numRows: Int, numCols: Int) = new ParMatrix[T](numRows, numCols)
  def apply[T:Numeric:Fractional:Manifest](xs: ParArray[T]*) = {
    val result = new ParMatrix[T](xs.length, xs(0).length) 
    var i = 0
    var offset = 0
    while(i < xs.length) {
      System.arraycopy(xs(i),0,result.data,offset,xs(i).length)
      offset += xs(i).length
      i += 1
    }
    result
  }

  def fromParArray[T:Numeric:Fractional:Manifest](x: ParArray[T], n: Int) = new ParMatrix[T](x,x.length/n, n)

}

class ParMatrix[@specialized T:Numeric:Fractional:Manifest](var data: ParArray[T], var numRows: Int, var numCols: Int) {

  def this(_numRows: Int, _numCols: Int) {
    this(new ParArray[T](_numRows*_numCols), _numRows, _numCols)
  }

  def apply(row: Int, col: Int): T = data(row * numCols + col)
  def update(row: Int, col: Int, newVal: T) { data(row * numCols + col) = newVal }

  def insertCol(pos: Int, x: ParVector[T]): ParMatrix[T] = {
    val newData = new ParArray[T](numRows * (numCols + 1))
    val newNumCols = numCols + 1
    var i = 0
    while (i < numRows){
      var col = 0
      var j = 0
      while (j < newNumCols) {
        if (j == pos) {
          newData(i*newNumCols+j) = x(i)
        }
        else{
          newData(i*newNumCols+j) = this(i,col)
          col += 1
        }
        j += 1
      }
      i += 1
    }
    data = newData
    numCols = newNumCols
    this
  }

  /*
  def **(m: ParMatrix[T]): ParMatrix[T] = {
    val result = ParMatrix[T](numRows, m.numCols)
    libscalaBLAS.matMult(data,m.data,result.data,
    result
  }
  */

  def *(m: ParMatrix[T]): ParMatrix[T] = {
    val result = ParMatrix[T](numRows, m.numCols)
    var i = 0
    while(i < numRows) {
      var j = 0
      while(j < m.numCols) {
        var acc: T = implicitly[Numeric[T]].zero
        var k = 0
        while(k < numCols) {
          acc += this(i,k) * m(k,j)
          k += 1
        }
        result(i,j) = acc
        j += 1
      }
      i += 1
    }
    result
  }

  def *(v: ParVector[T]): ParVector[T] = {
    val result = ParVector[T](numRows,false)
    var i = 0
    while(i < numRows) {
      var j = 0
      var acc: T = implicitly[Numeric[T]].zero
      while(j < numCols) {
        acc += this(i,j) * v(j)
        j += 1
      }
      result(i) = acc
      i += 1
    }
    result
  }

  def t() = { 
    val result = ParMatrix[T](numCols, numRows)
    var i = 0
    while(i < numRows) {
      var j = 0
      while(j < numCols) {
        result(j,i) = this(i,j)
        j += 1
      }
      i += 1
    }
    result
  }
}
