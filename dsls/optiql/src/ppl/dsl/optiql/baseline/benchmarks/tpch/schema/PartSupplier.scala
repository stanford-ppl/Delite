package ppl.dsl.optiql.baseline.benchmarks.tpch.schema
import collection.mutable.ArrayBuffer


class PartSupplier (
  key: Int,
  supplierKey: Int,
  availableQty: Int,
  supplyCost: Float,
  comment: String
)

class PartSupplierTable extends DataTable[PartSupplier] {

  var data: ArrayBuffer[PartSupplier] = new ArrayBuffer[PartSupplier]


  def addRecord(fs: Array[String]) {
    assert(fs.size == 5, "Expecting 5 fields, got: " + fs)
    val record = new PartSupplier(fs(0),fs(1),fs(2),fs(3),fs(4))
    data.append(record)
  }

  def instantiateTable() = new PartSupplierTable

  def numCols = 5

}