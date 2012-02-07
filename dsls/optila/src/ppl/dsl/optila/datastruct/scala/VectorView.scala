package ppl.dsl.optila.datastruct.scala

class VectorView[T:Manifest](x: Array[T], val start: Int, val stride: Int, val length: Int, __isRow: Boolean) { //extends VectorView[T]{

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

  //def Clone = { val v = new DenseVector[T](0, isRow); v.insertAll(0, this); v }

  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()

  // TODO: these semantics are ambiguous/ill-defined. e.g., copy on insert but write-through on update.
  // need to decide on a clean semantics and stick with it.
  // def sort(implicit o: Ordering[T]) = Clone.sort
  // def insert(pos:Int, x: T) = Clone.insert(pos,x)
  // def insertAll(pos: Int, xs: Vector[T]) = Clone.insertAll(pos,xs)
  // def copyFrom(pos: Int, xs: Vector[T]) = Clone.copyFrom(pos, xs)
  // def removeAll(pos: Int, len: Int) = Clone.removeAll(pos, len)
  // def trim = Clone.trim
  // def clear() = Clone.clear()

  protected def chkIndex(index: Int) = {
    if (index < 0 || index >= _data.length)
      throw new IndexOutOfBoundsException
    index
  }
}
