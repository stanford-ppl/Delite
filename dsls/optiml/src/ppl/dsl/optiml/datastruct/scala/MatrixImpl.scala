package ppl.dsl.optiml.datastruct.scala

object MatrixImpl {
}

class MatrixImpl[T:Manifest](nRows: Int, nCols: Int) extends Matrix[T] {
  import MatrixImpl._
  
  protected var _numRows = nRows
  protected var _numCols = nCols
  protected var _data: Array[T] = new Array[T](size)
    
  def numRows = _numRows
  def numCols = _numCols
  def size = _numRows*_numCols
  def data = _data
  
//  def apply(i: Int) : VectorView[T] = {
//    vview(i*numCols, 1, numCols, true)
//  }

  def apply(i: Int, j: Int) : T = {
    _data(i*numCols+j)
  }

  def update(row: Int, col: Int, x: T) {
    _data(row*numCols+col) = x
  }

  def dcApply(idx: Int) : T = _data(idx)
  def dcUpdate(idx: Int, x: T) { _data(idx) = x }

  def getRow(row: Int) = {
    new MatrixRowImpl[T](row, this, _data)
  }

  def getCol(col: Int) = {
    new MatrixColImpl[T](col, this, _data)
  }

  def vview(start: Int, stride: Int, length: Int, isRow: Boolean) = {
    new VectorViewImpl[T](_data, start, stride, length, isRow)
  }

  def cloneL = {
    val res = new MatrixImpl[T](numRows, numCols)
    for (i <- 0 until size){
      res.dcUpdate(i, _data(i))
    }
    res
  }

  def insertRow(pos: Int, x: Vector[T]) {
    //chkEquals(x._length, _numCols)
    val idx = pos*_numCols
    if (size == 0) _numCols = x.length
    insertSpace(idx, _numCols)
    for (i <- idx until idx+_numCols){
      _data(i) = x(i-idx)
    }
    _numRows += 1
  }

  def insertAllRows(pos: Int, xs: Matrix[T]) {
    //chkEquals(xs._numCols, _numCols)
    val idx = pos*_numCols
    if (size == 0) _numCols = xs.numCols
    val sz = _numCols*xs.numRows
    insertSpace(idx, sz)
    for (i <- idx until idx+sz){
      _data(i) = xs.dcApply(i-idx)
    }
    _numRows += xs.numRows
  }

  def insertCol(pos: Int, x: Vector[T]) {
    //chkEquals(x._length, _numRows)
    val newCols = _numCols+1
    if (size == 0) _numRows = x.length
    val out_data = new Array[T](_numRows*newCols)
    for (i <- 0 until _numRows){
      var col = 0
      for (j <- 0 until newCols) {
        if (j == pos){
          out_data(i*newCols+j) = x(i)
        }
        else{
          out_data(i*newCols+j) = this(i,col)
          col += 1
        }
      }
    }
    _data = out_data
    _numCols += 1
  }

  def insertAllCols(pos: Int, xs: Matrix[T]) {
    //m.chkEquals(xs._numRows, _numRows)
    val newCols = _numCols+xs.numCols
    if (size == 0) _numRows = xs.numRows
    val out_data = new Array[T](_numRows*newCols)
    for (i <- 0 until _numRows){
      var col = 0
      for (j <- 0 until newCols){
        if (j < pos || j >= pos+xs.numCols){
          out_data(i*newCols+j) = this(i,col)
          col += 1
        }
        else{
          out_data(i*newCols+j) = xs(i,j-pos)
        }
      }
    }
    _data = out_data

    _numCols += xs.numCols
  }

  def removeRows(pos: Int, num: Int) {
    val idx = pos*_numCols
    val len = num*_numCols
    System.arraycopy(_data, idx + len, _data, idx, size - (idx + len))
    _numRows -= num
  }

  def removeCols(pos:Int, num: Int) {
    val newCols = _numCols-num
    val out_data = new Array[T](_numRows*newCols)
    for (i <- 0 until _numRows){
      var col = 0
      for (j <- 0 until _numCols){
        if (j < pos || j >= pos+num){
          out_data(i*newCols+col) = this(i,j)
          col += 1
        }
      }
    }
    _data = out_data
    _numCols -= num
  }

  protected def ensureExtra(extra: Int) {
    if (_data.length - size < extra) {
      realloc(size + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = java.lang.Math.max(4, _data.length * 2)
    while (n < minLen) n *= 2
    val d = new Array[T](n)
    System.arraycopy(_data, 0, d, 0, size)
    _data = d
  }

  protected def insertSpace(pos: Int, len: Int) {
    if (pos < 0 || pos > size) throw new IndexOutOfBoundsException
    ensureExtra(len)
    System.arraycopy(_data, pos, _data, pos + len, size - pos)
  }

  protected def chkPos(index: Int) = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException
    index
  }

  
}

