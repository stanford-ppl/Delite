package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer


class Customer (
  key: Int,
  name: String,
  address: String,
  nationKey: Int,
  phone: String,
  accountBalance: Float,
  marketSegment: String,
  comment: String
)

class CustomerTable extends DataTable[Customer]  {

  var data = new ArrayBuffer[Customer]

  def addRecord(fs: Array[String]) {
    assert(fs.size == 8, "Expecting 8 fields, got: " + fs.toList.toString)
    val record = new Customer(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7))
    data.append(record)
  }

  def instantiateTable() = new CustomerTable

  def numCols = 8

}


