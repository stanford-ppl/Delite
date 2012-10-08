package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table

class Supplier(
  val key: Int,
  val name: String,
  val address: String,
  val nationKey: Int,
  val phone: String,
  val accountBalance: Float,
  val comment: String
)

class SupplierTable extends Table[Supplier] {


  def addRecord(fs: Array[String]) {
    assert(fs.size == 7, "Expecting 7 fields, got: " + fs)
    val record = new Supplier(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6))
    data.append(record)
  }

  def instantiateTable() = new SupplierTable


}
