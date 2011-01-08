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

class RangeVectorImpl(__start: Int, __end: Int, __stride: Int, __isRow: Boolean) extends Vector[Int] {

  protected var _start = __start
  protected var _end = __end
  protected var _stride = __stride
  protected var _isRow = __isRow

  def start = _start
  def end = _end
  def isRow = _isRow
  def stride = _stride
  def length = (_end-_start + _stride - 1)

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
