package ppl.dsl.optiml.datastruct.scala

/* A read-only RangeVector that does not allocate any data.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Nov 30, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class RangeVectorImpl(start: Int, end: Int, stride: Int, isrow: Boolean) extends Vector[Int] {
  def length = (end-start + stride - 1)

  protected var _isRow = isrow
  protected var _start = start
  protected var _stride = stride

  def isRow = _isRow

  override def apply(n: Int) : Int = {
    _start + n*_stride
  }

  // TODO: could make this a lazy initialization and allow updates,
  //       but update would be slow due to the check
  override def update(index: Int, x: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  def insert(pos: Int, x: Int): Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

}
