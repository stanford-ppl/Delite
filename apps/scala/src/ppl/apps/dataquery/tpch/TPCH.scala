package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import java.io.File

object TPCHRunner extends OptiQLApplicationRunner with TPCH

trait TPCH extends OptiQLApplication {

  val s = File.separator

  val tpchDataPath = "C:" + s + s + "vm_host"+ s + s +"tpch" + s + s + "debug"
  val debug = true

  def main() = {
    println("TPCH style benchmarking")

    //load TPCH data
    val lineItems = TPCH.loadLineItems(tpchDataPath)
    val res = lineItems Where(_.lineNumber < 5)
    lineItems.printAsTable()
    res.printAsTable()

  }

}