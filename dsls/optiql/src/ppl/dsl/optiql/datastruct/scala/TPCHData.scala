package ppl.dsl.optiql.datastruct.scala

import container.DataTable
import util.Date

object TPCHData {
  def loadCustomers(path: String) = { println("Loading Customers in Memory from " + path)}
  def loadLineItems(path: String) = { println("Loading Line Items in Memory from " + path)}
  def loadNations(path: String) = { println("Loading Nations in Memory from " + path)}
  def loadOrders(path: String) = { println("Loading Orders in Memory from " + path)}
  def loadParts(path: String) = { println("Loading Parts in Memory from " + path)}
  def loadPartSuppliers(path: String) = { println("Loading Part Suppliers in Memory from " + path)}
  def loadRegions(path: String) = { println("Loading Regions in Memory from " + path)}
  def loadSuppliers(path: String) = { println("Loading Suppliers in Memory from " + path)}
}

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

class CustomerTable extends DataTable[Customer]  {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 8, "Expecting 8 fields, got: " + fs.toList.toString)
    val record = new Customer(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7))
    data.append(record)
  }
  def instantiateTable() = new CustomerTable
}

class LineItem (
  val orderKey: Int,
  val partKey: Int,
  val supplierKey: Int,
  val lineNumber: Int,
  val quantity: Float,
  val extendedPrice: Float,
  val discount: Float,
  val tax: Float,
  val returnFlag: Char,
  val lineStatus: Char,
  val shipDate: Date,
  val commitDate: Date,
  val receiptDate: Date,
  val shipInstruct: String,
  val shipMode: String,
  val comment: String
)

class LineItemTable extends DataTable[LineItem] {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 16, "Expecting 16 fields, got: " + fs)
    val record = new LineItem(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8),fs(9),fs(10),fs(11),fs(12),fs(13),fs(14),fs(15))
    data.append(record)
  }
  def instantiateTable() = new LineItemTable
}

class Nation (
  val key: Int,
  val name: String,
  val regionKey: Int,
  val comment: String
)

class NationTable extends DataTable[Nation] {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 4, "Expecting 4 fields, got: " + fs)
    val record = new Nation(fs(0),fs(1),fs(2),fs(3))
    data.append(record)
  }
  def instantiateTable() = new NationTable
}

case class Order (
  val key: Int,
  val customerKey: Int,
  val status: Char,
  val totalPrice: Float,
  val date: Date,
  val priority: String,
  val clerk: String,
  val shipPriority: Int,
  val comment: String
)

class OrderTable extends DataTable[Order] {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Order(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    data.append(record)
  }
  def instantiateTable() = new OrderTable
}

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

class PartTable extends DataTable[Part]  {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Part(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    data.append(record)
  }
  def instantiateTable() = new PartTable
}

class PartSupplier (
  val key: Int,
  val supplierKey: Int,
  val availableQty: Int,
  val supplyCost: Float,
  val comment: String
)

class PartSupplierTable extends DataTable[PartSupplier] {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 5, "Expecting 5 fields, got: " + fs)
    val record = new PartSupplier(fs(0),fs(1),fs(2),fs(3),fs(4))
    data.append(record)
  }
  def instantiateTable() = new PartSupplierTable
}

class Region (
  val key: Int,
  val name: String,
  val comment: String
)

class RegionTable extends DataTable[Region] {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 3, "Expecting 3 fields, got: " + fs)
    val record = new Region(fs(0),fs(1),fs(2))
    data.append(record)
  }
  def instantiateTable() = new RegionTable
}

class Supplier(
  val key: Int,
  val name: String,
  val address: String,
  val nationKey: Int,
  val phone: String,
  val accountBalance: Float,
  val comment: String
)

class SupplierTable extends DataTable[Supplier] {
  def addRecord(fs: Array[String]) {
    assert(fs.size == 7, "Expecting 7 fields, got: " + fs)
    val record = new Supplier(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6))
    data.append(record)
  }
  def instantiateTable() = new SupplierTable
}