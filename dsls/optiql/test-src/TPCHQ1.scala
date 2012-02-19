package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import java.io.File

object TPCHQ1Runner extends OptiQLApplicationRunner with TPCHQ1

trait TPCHQ1 extends OptiQLApplication {

  val s = File.separator

  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }
  
  
  val debug = true

  def main() = {
  
    println("TPCH Q1 style benchmarking")
    if (args.length < 1) printUsage
    
    val tpchDataPath = args(0)
    
    //load TPCH data
    val lineItems = TPCH.loadLineItems(tpchDataPath)
    println("Loading Complete")
	  tic(lineItems.size)
    
    
    /*val res = lineItems.Select(e => new Result { val l_shipdate = e.l_shipdate })*/
      
    /*val res = lineItems.Select(e => new Result { val l_shipdate = e.l_shipdate; 
      val l_discount = e.l_discount }).Where(_.l_shipdate <= Date("1998-12-01"))*/
   
/*
    val res = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag,l.l_linestatus)) Select(g => new Result {
      val returnFlag = g.key._1
      val sumQty = g.Sum(_.l_quantity + 1.0)
      val sumCharge = g.Sum(l=> l.l_extendedprice * (1.0d - l.l_discount) * (1.0d + l.l_tax))
      val count = g.Count
    }) //OrderBy(_.returnFlag)
*/
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
    }) OrderBy(_.returnFlag) ThenBy(_.lineStatus)


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
    }) */
    
    toc(res)
    res.printAsTable()
  }
  
}
