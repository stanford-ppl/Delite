package ppl.dsl.optiql.baseline.benchmarks.tpch.schema
import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.baseline.containers.Table


class PartSupplier (
  val key: Int,
  val supplierKey: Int,
  val availableQty: Int,
  val supplyCost: Float,
  val comment: String
)

class PartSupplierTable extends Table[PartSupplier] {



  def addRecord(fs: Array[String]) {
    assert(fs.size == 5, "Expecting 5 fields, got: " + fs)
    val record = new PartSupplier(fs(0),fs(1),fs(2),fs(3),fs(4))
    data.append(record)
  }

  def instantiateTable() = new PartSupplierTable

}