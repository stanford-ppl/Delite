package ppl.dsl.optiml

object VectorImpl {

}

/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */
class VectorImpl[T: ClassManifest](len: Int, isRow: Boolean) extends Vector[T] {
  import VectorImpl._

  protected var _length = len
  protected var _is_row = isRow
  protected var _data: Array[T] = new Array[T](_length)

  def length = _length
  def is_row = _is_row

  def apply(n: Int) : T = {
    _data(n)
  }

  def update(index: Int, x: T) {
    _data(index) = x
  }

  def +=[A <: T](x: A): VectorImpl[T] = {
    ensureExtra(1)
    _data(_length) = x
    _length += 1
    this
  }
  
  protected def ensureExtra(extra: Int) {
    if (_data.length - _length < extra) {
      realloc(_length + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = 4 max (_data.length * 2)
    while (n < minLen) n *= 2
    val d = new Array[T](n)
    Array.copy(_data, 0, d, 0, _length)
    _data = d
  }

}
