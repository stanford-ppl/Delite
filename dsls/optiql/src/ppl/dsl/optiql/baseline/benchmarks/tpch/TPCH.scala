package ppl.dsl.optiql.baseline.benchmarks.tpch

import java.io.File
import schema._
import ppl.dsl.optiql.baseline.containers.Table
import ppl.dsl.optiql.baseline.util.{Date, Interval}
import ppl.dsl.optiql.baseline.OptiQL

object TPCH {

  val tpchDataPath="C:/vm_host/tpch/debug"
  val debug = true

  log("TPCH style benchmarking of OptiQL")

  //actual tables
  val customers = new CustomerTable;
  loadTPCHTable("customer",customers)
  val lineItems = new LineItemTable
  loadTPCHTable("lineitem", lineItems)
  val nations = new NationTable
  loadTPCHTable("nation", nations)
  val orders = new OrderTable
  loadTPCHTable("orders", orders)
  val parts = new PartTable
  loadTPCHTable("part", parts)
  val partSuppliers = new PartSupplierTable
  loadTPCHTable("partsupp", partSuppliers)
  val regions = new RegionTable
  loadTPCHTable("region", regions)
  val suppliers = new SupplierTable
  loadTPCHTable("supplier", suppliers)



  def main(args: Array[String]) {

   import OptiQL._

    //Execute TPC-H queries against my tables
    val q1 = lineItems Where(_.lineNumber < 5)
//    val q1 = lineItems Where(_.shipDate <= Date("1998-12-01") + Interval(90).days) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(g => new {
//      val returnFlag = g.key._1
//      val lineStatus = g.key._2
//      val sumQty = g.Sum(_.quantity)
//      val sumBasePrice = g.Sum(_.extendedPrice)
//      val sumDiscountedPrice = g.Sum(l => l.extendedPrice * (1-l.discount))
//      val sumCharge = g.Sum(l=> l.extendedPrice * (1-l.discount) * (1+l.tax))
//      val avgQty = g.Average(_.quantity)
//      val avgPrice = g.Average(_.extendedPrice)
//      val avgDiscount = g.Average(_.discount)
//      val countOrder = g.Count
//    }) OrderBy(_.lineStatus) ThenBy(_.returnFlag)


    println("TPCH Q1:")
    q1.printAsTable

    val q3 = customers.Where(_.marketSegment == "BUILDING").
      Join(orders)(_.key, _.customerKey, (customer, order)=> new {
      val orderKey = order.key
      val orderDate = order.date
      val orderShipPriority = order.shipPriority
    }).Join(lineItems)(_.orderKey, _.orderKey, (co, li)=> new {
      val orderKey = co.orderKey
      val orderDate = co.orderDate
      val orderShipPriority = co.orderShipPriority
      val orderShipDate = li.shipDate
      val extendedPrice = li.extendedPrice
      val discount = li.discount
    }).Where(col => col.orderDate < Date("1995-03-15") && col.orderShipDate < Date("1995-03-15")
    ).GroupBy(col => (col.orderKey,col.orderDate,col.orderShipPriority)) Select(g => new {
      val orderKey = g.key._1
      val revenue = g.Sum(e => e.extendedPrice * (1 - e.discount))
      val orderDate = g.key._2
      val shipPriority = g.key._2
    })


    println("TPCH Q3:")
    q3.printAsTable
  }

  def loadTPCHTable(path: String, table: Table[_]) = {
    log("loading tpch table from file[" + tpchDataPath + "/" + path +"] into memory")
    //open file for reading
    val filename = tpchDataPath + "/" + path + ".tbl"
    val file = new File(filename)
    if(file.isFile == false) throw new RuntimeException(filename + " doesn't appear to be a valid file")
    //load each line
    val records = scala.io.Source.fromFile(file).getLines()
    var i = 0
    while(records.hasNext) {
      val record = records.next
      val fields = record.split('|')
      table.addRecord(fields)
      i += 1
      if(i%10000 == 0) println("processed " + i + " records")
    }
    table
  }

  def log(msg: String) {
    if(debug) println(msg)
  }
}