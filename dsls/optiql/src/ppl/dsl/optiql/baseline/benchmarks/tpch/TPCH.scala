package ppl.dsl.optiql.baseline.benchmarks.tpch

import java.io.File
import schema._
import ppl.dsl.optiql.OptiQL

object TPCH {

  val tpchDataPath="C:/vm_host/tpch/debug"
  val debug = true

  log("TPCH style benchmarking of OptiQL")

  //actual tables
//  val customers = loadTPCHTable("customer")
//  val lineItems = loadTPCHTable("lineitem")
//  val nations = loadTPCHTable("nation")
  val orders = loadTPCHTable("orders")
//  val parts = loadTPCHTable("part")
//  val partSuppliers = loadTPCHTable("partsupp")
//  val regions = loadTPCHTable("region")
//  val suppliers = loadTPCHTable("supplier")



  def main(args: Array[String]) {

   import OptiQL._
    //Execute TPC-H queries against my tables
    //Q1
    val q1 = orders Where(o => o.status == 'O')

    println(q1)
  }



  def loadTPCHTable(path: String) = {
    log("loading tpch table from file[" + tpchDataPath + "/" + path +"] into memory")
    //open file for reading
    val filename = tpchDataPath + "/" + path + ".tbl"
    val file = new File(filename)
    if(file.isFile == false) throw new RuntimeException(filename + " doesn't appear to be a valid file")
    //declare array buffer of DataTable
    val table = instantiateTable(path)
    //load each line
    val records = scala.io.Source.fromFile(file).getLines()
    for(record <- records) {
      val fields = record.split('|')
      table.addRecord(fields)
    }
    table
  }

  //dispatch to appropriate instantiater
  def instantiateTable(path: String): DataTable[Order] =  path match {
//    case "customer" => new CustomerTable
//    case "lineitem" => new LineItemTable
//    case "nation"  => new NationTable
    case "orders" => new OrderTable
//    case "part" => new PartTable
//    case "partsupp" => new PartSupplierTable
//    case "region" => new RegionTable
//    case "supplier" => new SupplierTable
  }


  def log(msg: String) {
    if(debug) println(msg)
  }
}