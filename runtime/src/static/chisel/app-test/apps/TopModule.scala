// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class TopModuleTests(c: TopModule) extends PeekPokeTester(c) {

	val timeout = 300

	step(1)
	reset(1)

	// Enable hw accel
	poke(c.io.top_en, 1)

	// Run hw accel
    step(200)

    // Wait until done or timeout
    var done = peek(c.io.top_done)
    var numCycles = 0
    while ((done != 1) & (numCycles < timeout)) {
    	step(1)
    	numCycles += 1
    	done = peek(c.io.top_done)
    }
    // See if we had timeout or strange no-op run
    if ( (numCycles > timeout) | (numCycles < 2) ) {
      println("ERROR: Either timeout or did not run at all!")
      expect(c.io.top_done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
    }


}

class TopModuleTester extends ChiselFlatSpec {
  behavior of "TopModule"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new TopModule())(c => new TopModuleTests(c)) should be (true)
    }
  }
}
