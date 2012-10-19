package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import ppl.dsl.optiql.baseline.util.Date
import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table


class LineItem (
  val orderKey: Int,
  val partKey: Int,
  val supplierKey: Int,
  val lineNumber: Int,
  val quantity: Float,
  val extendedPrice: Float,
  val discount: Float,
  val tax: Float,
  val returnFlag: Char,
  val lineStatus: Char,
  val shipDate: Date,
  val commitDate: Date,
  val receiptDate: Date,
  val shipInstruct: String,
  val shipMode: String,
  val comment: String
)

class LineItemTable extends Table[LineItem] {



  def addRecord(fs: Array[String]) {
    assert(fs.size == 16, "Expecting 16 fields, got: " + fs)
    val record = new LineItem(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8),fs(9),fs(10),fs(11),fs(12),fs(13),fs(14),fs(15))
    data.append(record)
  }

  def instantiateTable() = new LineItemTable


}