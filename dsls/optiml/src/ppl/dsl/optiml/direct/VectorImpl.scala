package ppl.dsl.optiml.direct

object VectorImpl {

}

private class VectorImpl[T: ClassManifest](row_vec: Boolean, len: Int) extends Vector[T] {
  import VectorImpl._

  protected var _length = len
  protected var _is_row = row_vec
  protected var _data: Array[T] = new Array[T](_length)

  def length = _length
  def is_row = _is_row

  def apply(n: Int) : T = {
    _data(n)
  }

  def update(index: Int, x: T) {
    _data(index) = x
  }

  def +=[A <: T](x: A): Vector[T] = {
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
