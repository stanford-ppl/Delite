// See LICENSE.txt for license details.
package templates

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import chisel3.testers.BasicTester
import org.scalatest._
import org.scalatest.prop._

/**
 * Mem1D test harness
 */
class Mem1DTests(c: Mem1D) extends PeekPokeTester(c) {
  step(1)
  reset(1)
  for (i <- 0 until c.size ) {
    poke(c.io.w.addr, i)
    poke(c.io.w.data, i*2)
    poke(c.io.w.en, 1)
    step(1) 
    poke(c.io.w.en, 0)
    step(1)
  }

  for (i <- 0 until c.size ) {
    poke(c.io.r.addr, i)
    poke(c.io.r.en, 1)
    step(1)
    expect(c.io.output.data, i*2)
    poke(c.io.r.en, 0)
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
      c.io.w.addr.zip(List(i,j)).foreach { case (port, addr) => poke(port, addr) }
      poke(c.io.w.data, (i*c.dims(0) + j)*2)
      poke(c.io.w.en, 1)
      step(1) 
      poke(c.io.w.en, 0)
      step(1)
    }
  }

  for (i <- 0 until c.dims(0) ) {
    for (j <- 0 until c.dims(1) ) {
      c.io.r.addr.zip(List(i,j)).foreach { case (port, addr) => poke(port, addr) }
      poke(c.io.r.en, 1)
      step(1)
      expect(c.io.output.data, 2*(i*c.dims(0) + j))
      poke(c.io.r.en, 0)
      step(1)
    }
  }

}


/**
 * SRAM test harness
 */
class SRAMTests(c: SRAM) extends PeekPokeTester(c) {
  val depth = c.logicalDims.reduce{_*_}
  val N = c.logicalDims.length
  val numBufs = c.numBufs

  reset(1)
  poke(c.io.sel(0),1) // Select 0th writer

  // Write to each address
  for (i <- 0 until c.logicalDims(0)) { // Each row
    for (j <- 0 until c.logicalDims(1) by c.wPar) {
      // Set addrs
      (0 until c.numWriters).foreach{ writer => 
        (0 until c.wPar).foreach { kdim => 
          poke(c.io.w(writer*c.wPar+kdim).addr(0), i)
          poke(c.io.w(writer*c.wPar+kdim).addr(1), j+kdim)
          poke(c.io.w(writer*c.wPar+kdim).data, (i*c.logicalDims(0) + j + kdim)*2)
          poke(c.io.w(writer*c.wPar+kdim).en, true)
        }
      }
      step(1)
    }
  }
  // Turn off wEn
  (0 until c.numWriters).foreach{ writer => 
    (0 until c.wPar).foreach { kdim => 
      poke(c.io.w(writer*c.wPar+kdim).en, false)
    }
  }

  step(30)

  // Check each address
  for (i <- 0 until c.logicalDims(0)) { // Each row
    for (j <- 0 until c.logicalDims(1) by c.wPar) {
      // Set addrs
      (0 until c.numReaders).foreach{ writer => 
        (0 until c.rPar).foreach { kdim => 
          poke(c.io.r(kdim).addr(0), i)
          poke(c.io.r(kdim).addr(1), j+kdim)
          poke(c.io.r(kdim).en, true)
        }
      }
      step(1)
      (0 until c.rPar).foreach { kdim => 
        expect(c.io.output.data(kdim), (i*c.logicalDims(0) + j + kdim)*2)
      }
    }
  }
  // Turn off rEn
  (0 until numBufs).foreach{ writer => 
    (0 until c.rPar).foreach { kdim => 
      poke(c.io.r(kdim).en, true)
    }
  }

  step(1)


}

class Mem1DTester extends ChiselFlatSpec {
  behavior of "Mem1D"
  backends foreach {backend =>
    it should s"correctly do $backend" in {
      Driver(() => new Mem1D(1024))(c => new Mem1DTests(c)) should be (true)
    }
  }
}

// class MemNDTester extends ChiselFlatSpec {
//   behavior of "MemND"
//   backends foreach {backend =>
//     it should s"correctly do $backend" in {
//       Drivera(() => new MemND(List(4,8)))(c => new MemNDTests(c)) should be (true)
//     }
//   }
// }

class SRAMTester extends ChiselFlatSpec {
  behavior of "SRAM"
  backends foreach {backend =>
    it should s"correctly do $backend" in {
      Driver(() => new SRAM(List(16,16), 1, 32, 
                              List(1,2), List(1,1), 1, 1,
                              2, 2, "strided"))(c => new SRAMTests(c)) should be (true)
    }
  }
}
