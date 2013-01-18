package ppl.dsl.optiml.baseline

object SparseVector {
  def apply[A:Numeric:Manifest](length: Int) = new SparseVector[A](length)
}

class SparseVector[@specialized T:Numeric:Manifest](__length: Int) extends Vector[T] with SparseUtil {
  var _length = __length
  var _data = new Array[T](32)
  var _indices = new Array[Int](32)
  var _nnz = 0
  
  def newVector[B:Numeric:Manifest](len: Int): Vector[B] = SparseVector[B](len)
  
  def length: Int = _length
    
  def apply(pos: Int): T = {
    val offRaw = bsearch(_indices, 0, _nnz-1, pos)
    if (offRaw > -1) _data(offRaw) else implicitly[Numeric[T]].zero
  }
  
  def update(pos: Int, x: T): Unit = {
    val offRaw = bsearch(_indices, 0, _nnz-1, pos)
    if (offRaw > -1) _data(offRaw) = x
    else {
      if (x != implicitly[Numeric[T]].zero) {
        val off = ~offRaw
        insertSpace(off, 1)
        _indices(off) = pos
        _data(off) = x
      }
    }
  }
  
  protected def insertSpace(pos: Int, len: Int): Unit = {
    ensureExtra(len)
    System.arraycopy(_data, pos, _data, pos + len, _nnz - pos)
    System.arraycopy(_indices, pos, _indices, pos + len, _nnz - pos)
    _nnz += len
  }

  protected def ensureExtra(extra: Int): Unit = {
    if (_data.length - _nnz < extra) {
      realloc(_nnz+extra)
    }
  }

  protected def realloc(minLen: Int): Unit = {  
    var n = Math.max(4, _data.length * 2)
    while (n < minLen) n = n*2
    val newData = new Array[T](n)
    val newIndices = new Array[Int](n)
    System.arraycopy(_data, 0, newData, 0, _nnz)
    System.arraycopy(_indices, 0, newIndices, 0, _nnz)
    _data = newData
    _indices = newIndices
  }  
} 