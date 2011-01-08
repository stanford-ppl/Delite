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

  def insert(pos: Int, x: T): VectorImpl[T] = {
    insertSpace(pos, 1)
    _data(pos) = x    
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
