package ppl.dsl.optiml.datastruct.scala

/* A read-only RangeVector that does not allocate any data.
 *
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Nov 30, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class RangeVectorImpl(val start: Int, val end: Int, val stride: Int, __isRow: Boolean) extends RangeVector {

  protected var _isRow = __isRow

  val length = (end-start + stride - 1) / stride
  def isRow = _isRow

  // TODO (tiark): this crashes scalac for some reason
  //lazy val data = Array.range(start, end, stride)
  def data = throw new UnsupportedOperationException("there is a known bug with accessing data in RangeVectorImpl")

  def apply(n: Int) : Int = {
    start + n*stride
  }

  def mtrans = {
    _isRow = !_isRow
    this
  }

  def cloneL = { val v = new VectorImpl[Int](0, isRow); v.insertAll(0, this); v }

  def unsafeSetData(xs: Array[Int], len: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }
  

  def sort(implicit o: Ordering[Int]) = this

  // TODO: could make this a lazy initialization and allow updates,
  //       but update would be slow due to the check
  def update(index: Int, x: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def insert(pos: Int, x: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def insertAll(pos: Int, xs: Vector[Int]) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def copyFrom(pos: Int, xs: Vector[Int]) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def removeAll(pos: Int, len: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def trim {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def clear() {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def toList = {
    throw new UnsupportedOperationException("toList is not implemented on RangeVectorImpl yet")
  }
}
