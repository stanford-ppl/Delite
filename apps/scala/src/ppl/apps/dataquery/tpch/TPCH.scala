package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import java.io.File

object TPCHRunner extends OptiQLApplicationRunner with TPCH

trait TPCH extends OptiQLApplication {

  val s = File.separator

  val tpchDataPath = "C:" + s + s + "vm_host"+ s + s +"tpch" + s + s + "SF1"
  val debug = true

  def main() = {
    //println("TPCH style benchmarking")

    //load TPCH data
    val lineItems = TPCH.loadLineItems(tpchDataPath)
	tic(lineItems)
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
	})
	toc(res)
    //lineItems.printAsTable()
    res.printAsTable()
	
  }
  
  
  

}