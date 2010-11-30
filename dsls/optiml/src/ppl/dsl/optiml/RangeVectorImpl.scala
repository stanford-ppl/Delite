package ppl.dsl.optiml

/* A read-only RangeVector that does not allocate any data.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Nov 30, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class RangeVectorImpl(start: Int, end: Int, stride: Int, isRow: Boolean) extends VectorImpl[Int](end-start + stride - 1, isRow) {
  protected var _start = start
  protected var _stride = stride

  override def apply(n: Int) : Int = {
    _start + n*_stride
  }

  // TODO: could make this a lazy initialization and allow updates,
  //       but update would be slow due to the check
  override def update(index: Int, x: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def +=[A <: Int](x: A): VectorImpl[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

}