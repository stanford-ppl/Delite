package ppl.dsl.optiql.datastruct.scala.tpch

import ppl.dsl.optiql.datastruct.scala.liftables._
import ppl.dsl.optiql.datastruct.scala.container.DataTable
import ppl.dsl.optiql.datastruct.scala.util.Date
import java.io.File

object TPCHData {

  val debug = true

  def pathify(path:String, suffix: String) = path + File.separator + File.separator + suffix + ".tbl"

  def loadCustomers(path: String) = {  loadTPCHTable(pathify(path, "customer"), new CustomerTable)}
  def loadLineItems(path: String) = { loadTPCHTable(pathify(path, "lineitem"), new LineItemTable)}
  def loadNations(path: String) = { loadTPCHTable(pathify(path, "nation"), new NationTable)}
  def loadOrders(path: String) = { loadTPCHTable(pathify(path,"orders"), new OrderTable)}
  def loadParts(path: String) = { loadTPCHTable(pathify(path,"part"), new PartTable)}
  def loadPartSuppliers(path: String) = { loadTPCHTable(pathify(path,"partsupp"), new PartSupplierTable)}
  def loadRegions(path: String) = { loadTPCHTable(pathify(path,"region"), new RegionTable)}
  def loadSuppliers(path: String) = { loadTPCHTable(pathify(path,"supplier"), new SupplierTable)}

  def loadTPCHTable[T <: DataTable[_]](path: String, table: T): T = {
    log("loading tpch table from file[" + path + "] into memory")
    //open file for reading
    val file = new File(path)
    if(file.isFile == false) throw new RuntimeException(path + " doesn't appear to be a valid file")
    //load each line
    val records = scala.io.Source.fromFile(file).getLines()
    var i = 0
    while(records.hasNext) {
      val record = records.next
      val fields = record.split('|')
      table.addRecord(fields)
      i += 1
      if(i%500000 == 0) println("processed " + i + " records")
    }
    table
  }

  def log(msg: String) {
    if(debug) println(msg)
  }
}



class CustomerTable extends DataTable[Customer]  {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 8, "Expecting 8 fields, got: " + fs.toList.toString)
    val record = new Customer(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7))
    _data.append(record)
  }
  def instantiateTable() = new CustomerTable
}



class LineItemTable extends DataTable[LineItem] {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 16, "Expecting 16 fields, got: " + fs)
    val record = new LineItem(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8),fs(9),fs(10),fs(11),fs(12),fs(13),fs(14),fs(15))
    _data.append(record)
  }
  def instantiateTable() = new LineItemTable
}



class NationTable extends DataTable[Nation] {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 4, "Expecting 4 fields, got: " + fs)
    val record = new Nation(fs(0),fs(1),fs(2),fs(3))
    _data.append(record)
  }
  def instantiateTable() = new NationTable
}



class OrderTable extends DataTable[Order] {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Order(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    _data.append(record)
  }
  def instantiateTable() = new OrderTable
}



class PartTable extends DataTable[Part]  {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 9, "Expecting 9 fields, got: " + fs)
    val record = new Part(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8))
    _data.append(record)
  }
  def instantiateTable() = new PartTable
}



class PartSupplierTable extends DataTable[PartSupplier] {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 5, "Expecting 5 fields, got: " + fs)
    val record = new PartSupplier(fs(0),fs(1),fs(2),fs(3),fs(4))
    _data.append(record)
  }
  def instantiateTable() = new PartSupplierTable
}



class RegionTable extends DataTable[Region] {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 3, "Expecting 3 fields, got: " + fs)
    val record = new Region(fs(0),fs(1),fs(2))
    _data.append(record)
  }
  def instantiateTable() = new RegionTable
}



class SupplierTable extends DataTable[Supplier] {
  override def addRecord(fs: Array[String]) {
    assert(fs.size == 7, "Expecting 7 fields, got: " + fs)
    val record = new Supplier(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6))
    _data.append(record)
  }
  def instantiateTable() = new SupplierTable
}