// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class DelayerTests(c: Delayer) extends PeekPokeTester(c) {
  step(1)
  reset(1)
  var memory = Array.tabulate(c.length+1) { i => 0 }
  var head = 0
  var tail = if (c.length > 0) 1 else 0
  for (i <- 0 until 100) {
    val data = rnd.nextInt(10)
    memory(head) = data
    poke(c.io.input.data, data)
    // val a = peek(c.io.output.data)
    // println(s"writing $data to slot $head")
    // println(s"expect ${memory(tail)} in slot $tail and seeing $a")
    // println(s"")
    expect(c.io.output.data, memory(tail))
    head = if (head == c.length) 0 else head + 1
    tail = if (tail == c.length) 0 else tail + 1
    step(1)
  }


}

class DelayerTester extends ChiselFlatSpec {
  behavior of "Delayer"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Delayer(10))(c => new DelayerTests(c)) should be (true)
    }
  }
}


