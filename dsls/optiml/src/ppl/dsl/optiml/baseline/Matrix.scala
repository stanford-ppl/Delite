package ppl.dsl.optiml.baseline

trait Matrix[@specialized T] {
// abstract class Matrix[/*@specialized*/ T:Numeric:Manifest] {
  def numRows: Int
  def numCols: Int
  def apply(i: Int, j: Int): T  
  def newVector[B:Numeric:Manifest](len: Int): Vector[B]
  def vview(start: Int, stride: Int, length: Int): Vector[T]

  def apply(i: Int)(implicit m: Manifest[T], n: Numeric[T]): Vector[T] = {
    vview(i*numCols, 1, numCols)
    /*
    val out = newVector[T](numCols)
    for (j <- 0 until numCols) {
      out(j) = this(i,j)
    }
    out
    */
  }
  
  def mapRowsToVector[B:Numeric:Manifest](f: Vector[T] => B)(implicit m: Manifest[T], n: Numeric[T]) = {
    val out = newVector[B](numRows)
    for (i <- 0 until numRows) {
      out(i) = f(this(i))
    }
    out
  }
}