package ppl.dsl.optila.datastruct.scala

class SymmetricMatrixImpl[T:Manifest](n: Int) extends MatrixImpl[T](0,0) { //with SymmetricMatrix[T] {
  _numRows = n
  _numCols = n
  
  val realSize = n*n/2
  _data = new Array[T](realSize)
  
  override def dcSize = realSize
  
  def adjust(i: Int, j:Int) = if (i < j) (i, j) else (j, i)
  
  override def apply(i: Int, j: Int) : T = {    
    val (a, b) = adjust(i, j)
    _data(a*numCols+b)
  }

  override def update(i: Int, j: Int, x: T) {
    val (a, b) = adjust(i, j)
    _data(a*numCols+b) = x
  }
}
