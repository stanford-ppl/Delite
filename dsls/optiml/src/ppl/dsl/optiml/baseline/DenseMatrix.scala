package ppl.dsl.optiml.baseline

object DenseMatrix {
  def apply[A:Numeric:Manifest](numRows: Int, numCols: Int) = new DenseMatrix[A](numRows,numCols)
}

class DenseMatrix[@specialized T:Numeric:Manifest](__numRows: Int, __numCols: Int) extends Matrix[T] { 
  var _numRows = __numRows
  var _numCols = __numCols
  var _data: Array[T] = new Array[T](_numRows*_numCols)
  
  def numRows = _numRows
  def numCols = _numCols
  def size = numRows*numCols
  
  def newVector[B:Numeric:Manifest](len: Int): Vector[B] = DenseVector[B](len)
  def vview(start: Int, stride: Int, length: Int): Vector[T] = throw new UnsupportedOperationException("tbd") //new DenseVectorView[T](_data, start, stride, len)
  
  def apply(i: Int, j: Int): T  = {
    val offset = i*numCols+j
    _data(offset)
  }
  
  def update(i: Int, j: Int, y: T): Unit = {
    val offset = i*numCols+j
    _data(offset) = y
  }
  
  def insertCol(pos: Int, y: Vector[T]): Unit = {
    val newCols = numCols+1
    if (size == 0) _numRows = y.length
    val outData = new Array[T](numRows*newCols)
    for (i <- 0 until numRows){
      var col = 0
      for (j <- 0 until newCols) {
        if (j == pos){
          outData(i*newCols+j) = y(i)
        }
        else{
          outData(i*newCols+j) = this(i,col)
          col += 1
        }
      }
    }
    _data = outData
    _numCols = newCols
  }
  
}
