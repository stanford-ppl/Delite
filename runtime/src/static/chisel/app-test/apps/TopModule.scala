// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class TopModuleTests(c: TopModule) extends PeekPokeTester(c) {

  val input1 = 5
  val input2 = 5
  val input3 = 5
  val input4 = 5
  val input5 = 5
  val input6 = 5

  // Define 
  def InOutArg() {
    poke(c.io.ArgIn.Reg142, input1)
  }
  def InOutArgCheck() {
    expect(c.io.ArgOut.Reg143, input1+4)
  }
  def ScalarMath() {
    poke(c.io.ArgIn.Reg160, input1)
  }
  def ScalarMathCheck() {
    expect(c.io.ArgOut.Reg161, (input1*2+4)-1)
  }
  def MemTest1D() {
    poke(c.io.ArgIn.Reg160, input1)
  }
  def MemTest1DCheck() {
    expect(c.io.ArgOut.Reg161, (input1+383))
  }


  val timeout = 300

  step(1)
  reset(1)

  // Enable hw accel
  poke(c.io.top_en, 1)
  InOutArg() // Poke inputs

  // Wait until done or timeout
  var done = peek(c.io.top_done)
  var numCycles = 0
  while ((done != 1) & (numCycles < timeout)) {
    step(1)
    numCycles += 1
    done = peek(c.io.top_done)
  }
  poke(c.io.top_en, 0)
  // See if we had timeout or strange no-op run
  if ( (numCycles > timeout-1) | (numCycles < 2) ) {
    println("ERROR: Either timeout or did not run at all!")
    expect(c.io.top_done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }

  InOutArgCheck() // Expect outputs

}

class TopModuleTester extends ChiselFlatSpec {
  behavior of "TopModule"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new TopModule())(c => new TopModuleTests(c)) should be (true)
    }
  }
}

