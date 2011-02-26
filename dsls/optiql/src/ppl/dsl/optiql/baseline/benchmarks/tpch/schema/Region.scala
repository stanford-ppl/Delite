package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer


class Region (
  key: Int,
  name: String,
  comment: String
)

class RegionTable extends DataTable[Region] {

  var data: ArrayBuffer[Region] = new ArrayBuffer[Region]


  def addRecord(fs: Array[String]) {
    assert(fs.size == 3, "Expecting 3 fields, got: " + fs)
    val record = new Region(fs(0),fs(1),fs(2))
    data.append(record)
  }

  def instantiateTable() = new RegionTable

  def numCols = 3;

}