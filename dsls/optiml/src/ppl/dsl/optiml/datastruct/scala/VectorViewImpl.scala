package ppl.dsl.optiml.datastruct.scala

class VectorViewImpl[T:Manifest](x: Array[T], val start: Int, val stride: Int, val length: Int, __isRow: Boolean) extends VectorView[T]{

  protected var _data: Array[T] = x
  protected var _isRow = __isRow

  def isRow = _isRow
  def data = _data

  def idx(n: Int) = start + n*stride

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

  def toList = {
    _data.toList
  }

  def cloneL = { val v = new VectorImpl[T](0, isRow); v.insertAll(0, this); v }

  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()

  // TODO: these semantics are ambiguous/ill-defined. e.g., copy on insert but write-through on update.
  // need to decide on a clean semantics and stick with it.
  def sort(implicit o: Ordering[T]) = cloneL.sort
  def insert(pos:Int, x: T) = cloneL.insert(pos,x)
  def insertAll(pos: Int, xs: Vector[T]) = cloneL.insertAll(pos,xs)
  def copyFrom(pos: Int, xs: Vector[T]) = cloneL.copyFrom(pos, xs)
  def removeAll(pos: Int, len: Int) = cloneL.removeAll(pos, len)
  def trim = cloneL.trim
  def clear() = cloneL.clear()

  protected def chkIndex(index: Int) = {
    if (index < 0 || index >= _data.length)
      throw new IndexOutOfBoundsException
    index
  }
}
