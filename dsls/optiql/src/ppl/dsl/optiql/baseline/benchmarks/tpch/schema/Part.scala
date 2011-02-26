package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer

class Part(
  key:Int,
  name: String,
  mfgr: String,
  brand: String,
  pType: String,
  size: Int,
  container: String,
  retailPrice: Float,
  comment:String
)

class PartTable extends DataTable[Part]  {

  var data : ArrayBuffer[Part] = new ArrayBuffer[Part]

  def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Part(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    data.append(record)
  }

  def instantiateTable() = new PartTable

  def numCols = 9
}