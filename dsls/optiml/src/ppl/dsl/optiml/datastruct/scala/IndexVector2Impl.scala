package ppl.dsl.optiml.datastruct.scala

class IndexVector2Impl(rows: IndexVector, cols: IndexVector) {
  private var _rowInd: IndexVector = null
  private var _colInd: IndexVector = null

  // _rowInd means this IndexVector represents the rows of a Matrix, so it should be stored as a column vector
  _rowInd = rows
  if (_rowInd.isRow)  _rowInd = _rowInd.mtrans.asInstanceOf[IndexVector]
  _colInd = cols
  if (!_colInd.isRow) _colInd = _colInd.mtrans.asInstanceOf[IndexVector]

  def rowInd = _rowInd
  def colInd = _colInd
}