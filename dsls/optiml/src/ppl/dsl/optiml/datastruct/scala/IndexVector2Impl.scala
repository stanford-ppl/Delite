package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

// class IndexVector2Impl(rows: IndexVector, cols: IndexVector) extends IndexVector2 {
//   private var _rowInd: IndexVector = null
//   private var _colInd: IndexVector = null
// 
//   // _rowInd means this IndexVector represents the rows of a Matrix, so it should be stored as a column vector
//   // TODO: should be trans and not mtrans (need to update interface)
//   _rowInd = if (!rows.isInstanceOf[IndexVectorWC] && rows.isRow) rows.mtrans.asInstanceOf[IndexVector]
//             else rows
//   _colInd = if (!cols.isInstanceOf[IndexVectorWC] && !cols.isRow) cols.mtrans.asInstanceOf[IndexVector]
//             else cols
// 
//   def rowInd = _rowInd
//   def colInd = _colInd
// }