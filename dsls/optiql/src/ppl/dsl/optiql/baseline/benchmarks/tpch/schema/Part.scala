package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table

class Part(
  val key:Int,
  val name: String,
  val mfgr: String,
  val brand: String,
  val pType: String,
  val size: Int,
  val container: String,
  val retailPrice: Float,
  val comment:String
)

class PartTable extends Table[Part]  {


  def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Part(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    data.append(record)
  }

  def instantiateTable() = new PartTable

}