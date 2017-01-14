// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


class FIFOTests(c: FIFO) extends PeekPokeTester(c) {
  reset(1)
  step(5)
  var pushElement = 0
  var popElement = 0

  def push() {
    (0 until c.p).foreach { i => poke(c.io.in(i), pushElement + i) }
    poke(c.io.push, 1)
    step(1)
    poke(c.io.push,0)
    pushElement += c.p
  }
  def pop() {
    poke(c.io.pop, 1)
    (0 until c.p).foreach { i => expect(c.io.out(i), popElement + i) }
    step(1)
    popElement += c.p
  }

  // fill FIFO halfway
  for (i <- 0 until c.depth/c.p/2) {
    push()
  }

  // hold for a bit
  step(5)

  // pop FIFO halfway
  for (i <- 0 until c.depth/c.p/2) {
    pop()
  }
}
