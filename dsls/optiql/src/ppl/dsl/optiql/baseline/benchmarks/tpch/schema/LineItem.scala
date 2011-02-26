package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import ppl.dsl.optiql.baseline.util.Date
import collection.mutable.ArrayBuffer


class LineItem (
  orderKey: Int,
  partKey: Int,
  supplierKey: Int,
  lineNumber: Int,
  quantity: Float,
  extendedPrice: Float,
  discount: Float,
  tax: Float,
  returnFlag: Char,
  lineStatus: Char,
  shipDate: Date,
  commitDate: Date,
  receiptDate: Date,
  shipInstruct: String,
  shipMode: String,
  comment: String
)

class LineItemTable extends DataTable[LineItem] {

  var data = new ArrayBuffer[LineItem]


  def addRecord(fs: Array[String]) {
    assert(fs.size == 16, "Expecting 16 fields, got: " + fs)
    val record = new LineItem(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8),fs(9),fs(10),fs(11),fs(12),fs(13),fs(14),fs(15))
    data.append(record)
  }

  def instantiateTable() = new LineItemTable

  def numCols = 16

}