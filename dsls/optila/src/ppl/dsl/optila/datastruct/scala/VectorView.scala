package ppl.dsl.optila.datastruct.scala

class VectorView[T:Manifest](x: Array[T], val start: Int, val stride: Int, val length: Int, __isRow: Boolean) { 

  protected var _data: Array[T] = x
  protected var _isRow = __isRow

  def isRow = _isRow
  def data = _data

  private def idx(n: Int) = start + n*stride

  def apply(n: Int) : T = {
    _data(idx(n))
  }

  def update(n: Int, x: T) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()
}
