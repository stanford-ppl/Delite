package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import ppl.dsl.optiql.datastruct.scala.tpch._
import java.io.File

object TPCHQ1 extends OptiQLApplicationRunner with TPCHQ1Trait

trait TPCHBaseTrait extends OptiQLApplication {

  val queryName: String
  
  //var customers: Rep[CustomerTable] = _
  var lineItems: Rep[LineItemTable] = _
  //var orders: Rep[OrderTable] = _
  //var nations: Rep[NationTable] = _
  //var parts: Rep[PartTable] = _
  //var partSuppliers: Rep[PartSupplierTable] = _
  //var regions: Rep[RegionTable] = _
  //var suppliers: Rep[SupplierTable] = _
  
  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }
  
  def query(): Rep[_]
  
  
  val debug = true

  def main() = {
    println("TPCH style benchmarking " + queryName)
    if (args.length < 1) printUsage
    val tpchDataPath = args(0)
    
    //load TPCH data
    lineItems = TPCH.loadLineItems(tpchDataPath)
    println("Loading Complete")
	  tic(lineItems)
    query()
  }  
}


trait TPCHQ1Trait extends TPCHBaseTrait { 
  val queryName = "Q1"
  def query() = {        
    
    val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => l.l_returnflag) Select(g => new Result {
      val returnFlag = g.key     
      val sumQty = g.Sum(_.l_quantity)
      //val sumBasePrice = g.Sum(_.l_extendedprice)
      //val sumDiscountedPrice = g.Sum(l => l.l_extendedprice * (1.0d - l.l_discount))
      //val sumCharge = g.Sum(l=> l.l_extendedprice * (1.0d - l.l_discount) * (1.0d + l.l_tax))
      val avgQty = g.Average(_.l_quantity)
      //val avgPrice = g.Average(_.l_extendedprice)
      //val avgDiscount = g.Average(_.l_discount)
      //val countOrder = g.Count  
    })
   
    /*
    val res = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag,l.l_linestatus)) Select(g => new Result {
      val returnFlag = g.key._1
      val lineStatus = g.key._2
      val sumQty = g.Sum(_.l_quantity)
      val sumBasePrice = g.Sum(_.l_extendedprice)
      val sumDiscountedPrice = g.Sum(l => l.l_extendedprice * (1.0d - l.l_discount))
      val sumCharge = g.Sum(l=> l.l_extendedprice * (1.0d - l.l_discount) * (1.0d + l.l_tax))
      val avgQty = g.Average(_.l_quantity)
      val avgPrice = g.Average(_.l_extendedprice)
      val avgDiscount = g.Average(_.l_discount)
      val countOrder = g.Count            
    }) //*/
    
    toc(q)
    q.printAsTable()     
  
  }

}
