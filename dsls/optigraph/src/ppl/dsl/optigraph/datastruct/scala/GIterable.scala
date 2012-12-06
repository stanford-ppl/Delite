package ppl.dsl.optigraph.datastruct.scala

/**
 * Iterable collection of graph items (nodes or edges)
 */

class GIterable[@specialized T: ClassManifest](var __data: Array[T], __offset: Int, var __size: Int) {

  var _data = __data;
  var _offset = __offset;
  var _size = __size;

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen.
   */
  def unsafeSetData(xs: Array[T], len: Int) {
    _data = xs
    _size = len
  }
}