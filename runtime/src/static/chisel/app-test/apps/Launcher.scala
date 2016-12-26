// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import utils.AppRunner

object Launcher {
  val templates = Map(
	  "TopKernelLib" -> { (backendName: String) =>
	    Driver(() => new TopKernelLib(), "verilator") {
	      (c) => new TopKernelLibTests(c)
	    }
	  }
 )
  def main(args: Array[String]): Unit = {
    AppRunner(templates, args)
  }
}

