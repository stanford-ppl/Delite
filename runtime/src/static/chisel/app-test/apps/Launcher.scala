// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import utils.AppRunner

object Launcher {
  val templates = Map(
	  "TopModule" -> { (backendName: String) =>
	    Driver(() => new TopModule(), "verilator") {
	      (c) => new TopModuleTests(c)
	    }
	  }
 )
  def main(args: Array[String]): Unit = {
    AppRunner(templates, args)
  }
}

