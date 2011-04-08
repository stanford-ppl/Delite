package ppl.dsl.optiql.baseline.benchmarks.tpch

import java.io.File
import schema._
import ppl.dsl.optiql.OptiQL
import ppl.dsl.optiql.baseline.containers.DataTable
import ppl.dsl.optiql.baseline.util.{Date, Interval}

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
    //Q1
//    val q1 = orders Where(o => o.status == 'O' && o.key < 20) Select(o => new {
//      val key = o.key
//      val keyTwice = o.key * 2
//      val SuperStatus = "" + o.status + o.status
//    })
//
//
//    //println("Result of Q1:")
//    q1.printAsTable()
//
//    val q2 = orders GroupBy(_.status)
//
//    //println("Result of Q2:")
//    //q2.printAsTable()
//
//    val q3 = q2 Select(g => new {
//      val Status = g.key
//      val SummedPrices = g.Sum(_.totalPrice)
//      val AveragedPrices = g.Average(_.totalPrice)
//    })
//    q3.printAsTable

    val q1 = lineItems Where(_.shipDate <= Date("1998-12-01") + Interval(90).days) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(g => new {
      val returnFlag = g.key._1
      val lineStatus = g.key._2
      val sumQty = g.Sum(_.quantity)
      val sumBasePrice = g.Sum(_.extendedPrice)
      val sumDiscountedPrice = g.Sum(l => l.extendedPrice * (1-l.discount))
      val sumCharge = g.Sum(l=> l.extendedPrice * (1-l.discount) * (1+l.tax))
      val avgQty = g.Average(_.quantity)
      val avgPrice = g.Average(_.extendedPrice)
      val avgDiscount = g.Average(_.discount)
      val countOrder = g.Count

    })
    q1.printAsTable
  }

  def loadTPCHTable(path: String, table: DataTable[_]) = {
    log("loading tpch table from file[" + tpchDataPath + "/" + path +"] into memory")
    //open file for reading
    val filename = tpchDataPath + "/" + path + ".tbl"
    val file = new File(filename)
    if(file.isFile == false) throw new RuntimeException(filename + " doesn't appear to be a valid file")
    //load each line
    val records = scala.io.Source.fromFile(file).getLines()
    for(record <- records) {
      val fields = record.split('|')
      table.addRecord(fields)
    }
    table
  }

  def log(msg: String) {
    if(debug) println(msg)
  }
}