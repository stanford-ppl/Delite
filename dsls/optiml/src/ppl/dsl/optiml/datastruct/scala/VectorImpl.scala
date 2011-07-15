package ppl.dsl.optiml.datastruct.scala

object VectorImpl {
}

// TODO: replace Array.copy with System.arraycopy and 4.max() with a conditional

/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */
class VectorImpl[@specialized T: ClassManifest](__length: Int, __isRow: Boolean) extends Vector[T] {
  import VectorImpl._

  protected var _length = __length
  protected var _isRow = __isRow
  protected var _data: Array[T] = new Array[T](_length)

  def length = _length
  def isRow = _isRow
  def data = _data

  def this(__data: Array[T], __isRow: Boolean){
    this(0, __isRow)
    _data = __data
    _length = _data.length
  }

  def apply(n: Int): T = {
    _data(n)
  }

  def update(index: Int, x: T) {
    _data(index) = x
  }

  def cloneL = { val v = new VectorImpl[T](0, isRow); v.insertAll(0, this); v }

  def unsafeSetData(xs: Array[T], len: Int) {
    _data = xs
    _length = len
  }
  

  def sort(implicit o: Ordering[T]) = {
    val d = new Array[T](_length)
    System.arraycopy(_data, 0, d, 0, _length)
    scala.util.Sorting.quickSort(d)
    new VectorImpl[T](d, isRow)
  }

  def insert(pos: Int, x: T) {
    insertSpace(pos, 1)
    _data(pos) = x
  }

  def insertAll(pos: Int, xs: Vector[T]) {
    if (xs.isInstanceOf[EmptyVector[Any]]) return

    insertSpace(pos, xs.length)
    copyFrom(pos, xs)
  }

  def copyFrom(pos: Int, xs: Vector[T]) {
    //chkRange(pos, pos + xs.length)
    var i = 0
    while (i < xs.length) {
      _data(pos + i) = xs(i)
      i += 1
    }
  }

  def removeAll(pos: Int, len: Int) {
    //chkRange(pos, pos + len)
    System.arraycopy(_data, pos + len, _data, pos, _length - (pos + len))
    _length -= len
  }

  def trim {
    if (_length < _data.length) {
      val d = new Array[T](_length)
      System.arraycopy(_data, 0, d, 0, _length)
      _data = d
    }
  }
  
  def clear() {
    _length = 0
    _data = new Array[T](0)
  }

  def mtrans = {
    _isRow = !_isRow
    this
  }

  def toList = {
    _data.toList 
  }

  protected def insertSpace(pos: Int, len: Int) {
    ensureExtra(len)
    System.arraycopy(_data, pos, _data, pos + len, _length - pos)
    _length += len
  }

  protected def ensureExtra(extra: Int) {
    if (_data.length - _length < extra) {
      realloc(_length + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = java.lang.Math.max(4, _data.length * 2)
    while (n < minLen) n *= 2
    val d = new Array[T](n)
    System.arraycopy(_data, 0, d, 0, _length)
    _data = d
  }

}
