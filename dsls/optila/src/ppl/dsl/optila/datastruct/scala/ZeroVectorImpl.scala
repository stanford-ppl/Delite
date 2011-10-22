package ppl.dsl.optila.datastruct.scala

/*
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Mar 15, 2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
/*
abstract class ZeroVectorImpl[T:Manifest](val length: Int, __isRow: Boolean) extends ZeroVector[T] {

  protected var _isRow = __isRow

  def isRow = _isRow

  lazy val data = new Array[T](length)

  def update(index: Int, x: T) {
    throw new UnsupportedOperationException("Zero vectors cannot be updated; try cloning first")
  }

  def unsafeSetData(xs: Array[T], len: Int) {
    throw new UnsupportedOperationException("Zero vectors cannot be updated; try cloning first")
  }
  
  def mtrans = {
    _isRow = !_isRow
    this
  }

  def cloneL = new VectorImpl[T](length, isRow)
  def sort(implicit o: Ordering[T]) = this
  def insert(pos:Int, x: T) = cloneL.insert(pos,x)
  def insertAll(pos: Int, xs: Vector[T]) = cloneL.insertAll(pos,xs)
  def copyFrom(pos: Int, xs: Vector[T]) = cloneL.copyFrom(pos, xs)
  def removeAll(pos: Int, len: Int) = cloneL.removeAll(pos, len)
  def trim = cloneL.trim
  def clear() = cloneL.clear()
  def toList = data.toList
}

class ZeroVectorIntImpl(__length: Int, __isRow: Boolean) extends ZeroVectorImpl[Int](__length, __isRow) {
  def apply(n: Int) = 0
}

class ZeroVectorDoubleImpl(__length: Int, __isRow: Boolean) extends ZeroVectorImpl[Double](__length, __isRow) {
  def apply(n: Int) = 0.0
}

class ZeroVectorFloatImpl(__length: Int, __isRow: Boolean) extends ZeroVectorImpl[Float](__length, __isRow) {
  def apply(n: Int) = 0f
}
*/