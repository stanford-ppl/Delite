package ppl.dsl.optiml.datastruct.scala

object MatrixImpl {
  def getDoubleManifest = classManifest[Double]
}

class MatrixImpl[@specialized T: ClassManifest](nRows: Int, nCols: Int) extends Matrix[T] {
  import MatrixImpl._
  
  protected var _numRows = nRows
  protected var _numCols = nCols
  protected var _data: Array[T] = new Array[T](size)
    
  def numRows = _numRows
  def numCols = _numCols
  def size = _numRows*_numCols
  def data = _data

  def doubleData = _data.asInstanceOf[Array[Double]]
  
  def apply(i: Int) : VectorViewImpl[T] = {
    vview(i*numCols, 1, numCols, true)
  }

  def apply(i: Int, j: Int) : T = {
    _data(i*numCols+j)
  }

  def update(row: Int, col: Int, x: T) = {
    _data(row*numCols+col) = x
  }

  def dcApply(idx: Int) : T = _data(idx)
  def dcUpdate(idx: Int, x: T) = { _data(idx) = x }

  def vview(start: Int, stride: Int, length: Int, isRow: Boolean) : VectorViewImpl[T] = {
    new VectorViewImpl[T](_data, start, stride, length, isRow)
  }

  def insertRow(pos: Int, x: Vector[T]): MatrixImpl[T] = {
    val idx = pos*_numCols
    insertSpace(idx, _numCols)
    for (i <- idx until idx+_numCols){
      _data(i) = x(i-idx)
    }
    _numRows += 1
    this
  }

  protected def ensureExtra(extra: Int) {
    if (_data.length - size < extra) {
      realloc(size + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = 4 max (_data.length * 2)
    while (n < minLen) n *= 2
    val d = new Array[T](n)
    Array.copy(_data, 0, d, 0, size)
    _data = d
  }

  protected def insertSpace(pos: Int, len: Int) {
    if (pos < 0 || pos > size) throw new IndexOutOfBoundsException
    ensureExtra(len)
    Array.copy(_data, pos, _data, pos + len, size - pos)
  }

  protected def chkPos(index: Int) = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException
    index
  }

  
}

