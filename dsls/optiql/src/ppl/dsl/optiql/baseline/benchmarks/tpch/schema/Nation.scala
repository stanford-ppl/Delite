package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table


class Nation (
  val key: Int,
  val name: String,
  val regionKey: Int,
  val comment: String
)

class NationTable extends Table[Nation] {


  def addRecord(fs: Array[String]) {
    assert(fs.size == 4, "Expecting 4 fields, got: " + fs)
    val record = new Nation(fs(0),fs(1),fs(2),fs(3))
    data.append(record)
  }

  def instantiateTable() = new NationTable

}