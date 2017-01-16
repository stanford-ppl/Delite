// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


class FIFOTests(c: FIFO) extends PeekPokeTester(c) {
  reset(1)
  step(5)
  var pushElement = 0
  var popElement = 0

  def push(inc: Boolean = true) {
    (0 until c.p).foreach { i => poke(c.io.in(i), pushElement + i) }
    poke(c.io.push, 1)
    step(1)
    poke(c.io.push,0)
    if (inc) pushElement += c.p
  }
  def pop(inc: Boolean = true) {
    poke(c.io.pop, 1)
    val a = peek(c.io.out(0))
    step(1)
    poke(c.io.pop,0)
    if (inc) {
      val a = peek(c.io.out(0))
      println(s"Expect $popElement, got $a (error ${a != popElement})")
      (0 until c.p).foreach { i => expect(c.io.out(i), popElement + i) }
      popElement += c.p      
    }
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

  // pop to overflow
  expect(c.io.debug.overflow, 0)
  pop(false)
  expect(c.io.debug.overflow, 1)
  push(false)
  expect(c.io.debug.overflow, 0)
  push()

  // randomly push 'n pop
  val numTransactions = c.depth*10
  for (i <- 0 until numTransactions) {
    val newenable = rnd.nextInt(3)
    if (newenable == 1 | pushElement - popElement < 2*c.p) push()
    else if (newenable == 2 | pushElement - popElement >= c.depth/c.p-c.p ) pop()
    else step(1)
  }
  
}
