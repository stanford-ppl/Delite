package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import ppl.dsl.optiql.baseline.util.Date
import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table

case class Order (
  val key: Int,
  val customerKey: Int,
  val status: Char,
  val totalPrice: Float,
  val date: Date,
  val priority: String,
  val clerk: String,
  val shipPriority: Int,
  val comment: String
)

class OrderTable extends Table[Order] {



  def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Order(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    data.append(record)
  }

  def instantiateTable() = new OrderTable
}