// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class TopModuleTests(c: TopModule) extends PeekPokeTester(c) {

    step(200)

}

class TopModuleTester extends ChiselFlatSpec {
  behavior of "TopModule"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new TopModule())(c => new TopModuleTests(c)) should be (true)
    }
  }
}
