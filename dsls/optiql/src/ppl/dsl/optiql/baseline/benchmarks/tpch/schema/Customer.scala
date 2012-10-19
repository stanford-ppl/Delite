package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table


class Customer (
  val key: Int,
  val name: String,
  val address: String,
  val nationKey: Int,
  val phone: String,
  val accountBalance: Float,
  val marketSegment: String,
  val comment: String
)

class CustomerTable extends Table[Customer]  {


  def addRecord(fs: Array[String]) {
    assert(fs.size == 8, "Expecting 8 fields, got: " + fs.toList.toString)
    val record = new Customer(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7))
    data.append(record)
  }

  def instantiateTable() = new CustomerTable


}


