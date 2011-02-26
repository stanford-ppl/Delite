package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer


class Nation (
  key: Int,
  name: String,
  regionKey: Int,
  comment: String
)

class NationTable extends DataTable[Nation] {

  var data = new ArrayBuffer[Nation]

  def addRecord(fs: Array[String]) {
    assert(fs.size == 4, "Expecting 4 fields, got: " + fs)
    val record = new Nation(fs(0),fs(1),fs(2),fs(3))
    data.append(record)
  }

  def instantiateTable() = new NationTable

  def numCols = 4
}