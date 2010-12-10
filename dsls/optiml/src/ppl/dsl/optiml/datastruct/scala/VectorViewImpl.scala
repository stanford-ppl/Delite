package ppl.dsl.optiml.datastruct.scala

class VectorViewImpl[T : ClassManifest](x: Array[T], offset: Int, str: Int, len: Int, row_vec: Boolean) extends VectorView[T]{

  protected var _data: Array[T] = x
  protected var _length = len
  protected var _is_row = row_vec
  protected var _start = offset
  protected var _stride = str

  def start = _start
  def stride = _stride
  def length = _length
  def is_row = _is_row  

  def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : T = {
    _data(chkIndex(idx(n)))
  }

  def update[A <: T](n: Int, x: A) {
    _data(chkIndex(idx(n))) = x
  }

  def insert[A <: T](pos:Int, x: A): VectorViewImpl[T] = {
    throw new UnsupportedOperationException("operations on views not supported yet")
  }

  protected def chkIndex(index: Int) = {
    if (index < 0 || index >= _data.length)
      throw new IndexOutOfBoundsException
    index
  }
}
