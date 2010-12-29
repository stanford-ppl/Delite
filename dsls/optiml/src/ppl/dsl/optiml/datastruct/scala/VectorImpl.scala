package ppl.dsl.optiml.datastruct.scala

object VectorImpl {
  def getDoubleManifest = classManifest[Double]
}

/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */
class VectorImpl[@specialized T: ClassManifest](len: Int, isrow: Boolean) extends Vector[T] {
  import VectorImpl._

  protected var _length = len
  protected var _isRow = isrow
  protected var _data: Array[T] = new Array[T](_length)

  def length = _length
  def isRow = _isRow
  def data = _data
  def doubleData: Array[Double] = _data.asInstanceOf[Array[Double]]

  def apply(n: Int) : T = {
    _data(n)
  }

  def update(index: Int, x: T) {
    _data(index) = x
  }

  override def clone = { val v = new VectorImpl[T](0, isRow); v.insertAll(0, this); v }

  def insert(pos: Int, x: T) {
    insertSpace(pos, 1)
    _data(pos) = x
  }

  def insertAll(pos: Int, xs: Vector[T]) {
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
    Array.copy(_data, pos + len, _data, pos, _length - (pos + len))
    _length -= len
  }

  def trim {
    if (_length < _data.length) {
      val d = new Array[T](_length)
      Array.copy(_data, 0, d, 0, _length)
      _data = d
    }
  }

  def mtrans = {
    _isRow = !_isRow
    this
  }

  protected def insertSpace(pos: Int, len: Int) {
    ensureExtra(len)
    Array.copy(_data, pos, _data, pos + len, _length - pos)
    _length += len
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
