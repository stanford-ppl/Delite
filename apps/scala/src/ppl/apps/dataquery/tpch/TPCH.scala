    package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import java.io.File

object TPCHRunner extends OptiQLApplicationRunner with TPCH

trait TPCH extends OptiQLApplication {

  val s = File.separator

  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }
  
  
  val debug = true

  def main() = {
  
  
    //println("TPCH style benchmarking")
    if (args.length < 1) printUsage
    
    val tpchDataPath = args(0)
    

    //load TPCH data
    val lineItems = TPCH.loadLineItems(tpchDataPath)
    type Result = Row[Rep]

	tic(lineItems)
    val res = lineItems Select(e => new Result { val shipDate = e.shipDate  })
    
    /*
    val res = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) */
    toc(res)
    res.printAsTable()
/*
    val res1 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res2 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res3 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res4 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res5 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res6 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res7 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res8 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res9 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    val res10 = lineItems Where(_.shipDate <= Date("1998-12-01")) GroupBy(l => (l.returnFlag,l.lineStatus)) Select(e => {
      val returnFlag = e.key._1
      val lineStatus = e.key._2
      val sumQty = e.Sum(_.quantity)
      val sumBasePrice = e.Sum(_.extendedPrice)
      val sumDiscountedPrice = e.Sum(l => l.extendedPrice * (1.0f-l.discount))
      val sumCharge = e.Sum(l=> l.extendedPrice * (1.0f-l.discount) * (1.0f+l.tax))
      val avgQty = e.Average(_.quantity)
      val avgPrice = e.Average(_.extendedPrice)
      val avgDiscount = e.Average(_.discount)
      val count = e.Count
      //hack
      ResultQ1(returnFlag, lineStatus, sumQty, sumBasePrice, sumDiscountedPrice, sumCharge, avgQty, avgPrice, avgDiscount, count)
    }) 
    toc(res1,res2,res3,res4,res5,res6,res7,res8,res9,res10)
    res1.printAsTable()
*/    
    


	
  }
  
  
  

}
