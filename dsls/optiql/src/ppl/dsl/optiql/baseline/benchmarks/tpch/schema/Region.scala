package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table


class Region (
  val key: Int,
  val name: String,
  val comment: String
)

class RegionTable extends Table[Region] {



  def addRecord(fs: Array[String]) {
    assert(fs.size == 3, "Expecting 3 fields, got: " + fs)
    val record = new Region(fs(0),fs(1),fs(2))
    data.append(record)
  }

  def instantiateTable() = new RegionTable


}