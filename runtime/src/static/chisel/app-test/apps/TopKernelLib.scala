// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class TopKernelLibTests(c: TopKernelLib) extends PeekPokeTester(c) {

    step(200)

}

class TopKernelLibTester extends ChiselFlatSpec {
  behavior of "TopKernelLib"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new TopKernelLib())(c => new TopKernelLibTests(c)) should be (true)
    }
  }
}
