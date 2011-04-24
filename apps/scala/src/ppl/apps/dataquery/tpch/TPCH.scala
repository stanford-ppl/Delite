package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}

object TPCHRunner extends OptiQLApplicationRunner with TPCH

trait TPCH extends OptiQLApplication {

  val tpchDataPath = "C:/vm_host/tpch/debug"
  val debug = true

  def main() = {
    println("TPCH style benchmarking")

    //load TPCH data
    val customers = TPCH.loadCustomers(tpchDataPath)

  }

}