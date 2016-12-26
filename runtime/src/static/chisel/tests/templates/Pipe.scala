// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class PipeTests(c: Pipe) extends PeekPokeTester(c) {
  step(1)
  reset(1)
  poke(c.io.input.enable, 1)
  val timeout = 999

  val maxes = (0 until c.ctrDepth).map { i => math.abs(rnd.nextInt(20)) + 2 } 
  maxes.map { a => println("max of ctr = " + a) }
  var cnts = Array.tabulate(c.ctrDepth) { i => 0 }

  (0 until c.ctrDepth).foreach { i => poke(c.io.input.sm_maxIn(i), maxes(i))}

  def handleStep {
    val cnt_en = peek(c.io.output.ctr_en).toInt
    if (cnt_en == 1) {
      cnts(0) += 1
      // println(s"cnts ${cnts(0)} ${cnts(1)}")
      (0 until c.ctrDepth-1).foreach { i =>
        val m = peek(c.io.output.ctr_maxOut(i))
        if (cnts(i) >= m) {
          cnts(i+1) += 1
          cnts(i) = 0
        }
      }
      val last = peek(c.io.output.ctr_maxOut(c.ctrDepth-1))
      if (cnts(c.ctrDepth-1) == last) {
        poke(c.io.input.ctr_done, 1)
      }
    }
    step(1)
    poke(c.io.input.ctr_done,0)
  }

  var numEnCycles = 0
  var done = peek(c.io.output.done).toInt
  while (done != 1) {
    handleStep
    done = peek(c.io.output.done).toInt
    val cnt_en = peek(c.io.output.ctr_en).toInt
    if (cnt_en == 1) numEnCycles += 1
  }
  poke(c.io.input.enable, 0)
  if ( (numEnCycles > timeout) | (numEnCycles < 2) ) {
    println("ERROR: Either timeout or did not run at all!")
    expect(c.io.output.done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }
  val expectedCycs = maxes.reduce{_*_}
  if ( numEnCycles != expectedCycs ) {
    println(s"ERROR: Ran ${numEnCycles} but expected ${expectedCycs} cycs!")
    expect(c.io.output.done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }
  println(s"numiters $numEnCycles")
  step(20)

  poke(c.io.input.enable, 1)
  numEnCycles = 0
  done = peek(c.io.output.done).toInt
  (0 until c.ctrDepth).foreach { i => cnts(i) = 0 }
  while (done != 1) {
    handleStep
    done = peek(c.io.output.done).toInt
    val cnt_en = peek(c.io.output.ctr_en).toInt
    if (cnt_en == 1) numEnCycles += 1
  }
  poke(c.io.input.enable, 0)
  if ( (numEnCycles > timeout) | (numEnCycles < 2) ) {
    println("ERROR: Either timeout or did not run at all!")
    expect(c.io.output.done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }
  if ( numEnCycles != expectedCycs ) {
    println(s"ERROR: Ran ${numEnCycles} but expected ${expectedCycs} cycs!")
    expect(c.io.output.done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }
  println(s"numiters $numEnCycles")

}

class PipeTester extends ChiselFlatSpec {
  behavior of "Pipe"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Pipe(2))(c => new PipeTests(c)) should be (true)
    }
  }
}
