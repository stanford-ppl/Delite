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
  
  
    println("TPCH style benchmarking")
    if (args.length < 1) printUsage
    
    val tpchDataPath = args(0)
    
    
/*    
    class Result extends Row[Rep]
    
    class ApplyDynamicOps {
      def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
    }
    implicit def applyDynamicOps[T <: Result](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps 
    
    
    val qual = new Result{ val xxx: Rep[Int] = 5    }
    val x: Rep[Int] = qual.xxx("foo",unit(5),unit(4)) 
    x 
  */  

    //load TPCH data
    val lineItems = TPCH.loadLineItems(tpchDataPath)
    println("Loading Complete")
	  tic(lineItems)
    
    
      
    //val res = lineItems Select(e => new Result { val l_shipdate = e.l_shipdate  }) Where(_.l_shipdate <= Date("1998-12-01"))
   
    ///*
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
