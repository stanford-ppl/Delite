package ppl.dsl.optiml.datastruct.scala

class VectorViewImpl[@specialized T: ClassManifest](x: Array[T], offset: Int, str: Int, len: Int, row_vec: Boolean) extends VectorView[T]{

  protected var _data: Array[T] = x
  protected var _length = len
  protected var _isRow = row_vec
  protected var _start = offset
  protected var _stride = str

  def start = _start
  def stride = _stride
  def length = _length
  def isRow = _isRow

  def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : T = {
    _data(idx(n))
  }

  def update(n: Int, x: T) {
    _data(idx(n)) = x
  }

  def mtrans = {
    _isRow = !_isRow
    this
  }

  override def clone = { val v = new VectorImpl[T](0, isRow); v.insertAll(0, this); v }

  // TODO: these semantics are ambiguous/ill-defined. e.g., copy on insert but write-through on update.
  // need to decide on a clean semantics and stick with it.
  def insert(pos:Int, x: T) = clone.insert(pos,x)
  def insertAll(pos: Int, xs: Vector[T]) = clone.insertAll(pos,xs)
  def copyFrom(pos: Int, xs: Vector[T]) = clone.copyFrom(pos, xs)
  def removeAll(pos: Int, len: Int) = clone.removeAll(pos, len)
  def trim = clone.trim

  protected def chkIndex(index: Int) = {
    if (index < 0 || index >= _data.length)
      throw new IndexOutOfBoundsException
    index
  }
}
