package ppl.dsl.optila.datastruct.scala

class DenseVectorView[T:Manifest](val _data: Array[T], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 

  private def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : T = {
    _data(idx(n))
  }

  def update(n: Int, x: T) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()
}
