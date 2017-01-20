// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


class FIFOTests(c: FIFO) extends PeekPokeTester(c) {
  reset(1)
  step(5)
  var pushElement = 0
  var popElement = 0
  val p = scala.math.max(c.pW, c.pR)

  def push(inc: Boolean = true) {
    (0 until c.pW).foreach { i => poke(c.io.in(i), pushElement + i) }
    poke(c.io.push, 1)
    step(1)
    poke(c.io.push,0)
    if (inc) pushElement += c.pW
  }
  def pop(inc: Boolean = true) {
    poke(c.io.pop, 1)
    if (inc) {
      // val a = peek(c.io.out(0))
      // println(s"Expect $popElement, got $a (error ${a != popElement})")
      (0 until c.pR).foreach { i => expect(c.io.out(i), popElement + i) }
      popElement += c.pR      
    }
    step(1)
    poke(c.io.pop,0)
  }

  // fill FIFO halfway
  for (i <- 0 until c.depth/c.pW/2) {
    push()
  }

  // hold for a bit
  step(5)

  // pop FIFO halfway
  for (i <- 0 until c.depth/c.pR/2) {
    pop()
  }

  // pop to overread
  expect(c.io.debug.overread, 0)
  (0 until (p/c.pR)).foreach{ i => pop(false) }
  expect(c.io.debug.overread, 1)
  (0 until (p/c.pW)).foreach{ i => push(false) }
  expect(c.io.debug.overread, 0)
  (0 until c.depth/c.pW).foreach { i => push(false) }
  expect(c.io.debug.overwrite, 0)
  (0 until (p/c.pW)).foreach{ i => push(false) }
  expect(c.io.debug.overwrite, 1)
  (0 until (p/c.pR)).foreach{ i => pop(false) }
  expect(c.io.debug.overwrite, 0)
  (0 until c.depth/c.pR).foreach { i => pop(false) }



  // randomly push 'n pop
  val numTransactions = c.depth*10
  for (i <- 0 until numTransactions) {
    val newenable = rnd.nextInt(4)
    if (pushElement - popElement < 2*p) push()
    else if (pushElement - popElement >= (c.depth/p)-p) pop()
    else if (newenable == 1) push()
    else if (newenable == 2) pop()
    else step(1)
  }
  
}
