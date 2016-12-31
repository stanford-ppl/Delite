// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * Mem1D test harness
 */
class Mem1DTests(c: Mem1D) extends PeekPokeTester(c) {
  step(1)
  reset(1)
  for (i <- 0 until c.size ) {
    poke(c.io.input.wAddr, i)
    poke(c.io.input.wData, i*2)
    poke(c.io.input.wEn, 1)
    step(1) 
    poke(c.io.input.wEn, 0)
    step(1)
  }

  for (i <- 0 until c.size ) {
    poke(c.io.input.rAddr, i)
    poke(c.io.input.rEn, 1)
    step(1)
    expect(c.io.output.rData, i*2)
    poke(c.io.input.rEn, 0)
    step(1)
  }

}

/**
 * MemND test harness
 */
class MemNDTests(c: MemND) extends PeekPokeTester(c) {
  val depth = c.dims.reduce{_*_}
  val N = c.dims.length

  step(1)
  reset(1)
  // Assume only 2D
  for (i <- 0 until c.dims(0)+1 ) {
    for (j <- 0 until c.dims(1) ) {
      c.io.input.wAddr.zip(List(i,j)).foreach { case (port, addr) => poke(port, addr) }
      poke(c.io.input.wData, (i*c.dims(0) + j)*2)
      poke(c.io.input.wEn, 1)
      step(1) 
      poke(c.io.input.wEn, 0)
      step(1)
    }
  }

  for (i <- 0 until c.dims(0) ) {
    for (j <- 0 until c.dims(1) ) {
      c.io.input.rAddr.zip(List(i,j)).foreach { case (port, addr) => poke(port, addr) }
      poke(c.io.input.rEn, 1)
      step(1)
      expect(c.io.output.rData, 2*(i*c.dims(0) + j))
      poke(c.io.input.rEn, 0)
      step(1)
    }
  }

}


class Mem1DTester extends ChiselFlatSpec {
  behavior of "Mem1D"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Mem1D(1024))(c => new Mem1DTests(c)) should be (true)
    }
  }
}

class MemNDTester extends ChiselFlatSpec {
  behavior of "MemND"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new MemND(List(4,8)))(c => new MemNDTests(c)) should be (true)
    }
  }
}
