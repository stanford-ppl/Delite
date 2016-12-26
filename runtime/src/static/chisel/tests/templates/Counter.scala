// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * Counter test harness
 */

class CounterTests(c: Counter) extends PeekPokeTester(c) {

  // def check(wire: Any, value: Any, printon: Bool) {
  //   val a = peek(wire)
  //   if (printon) println("Expect " + a + " to be " + value)
  //   expect(wire, value)
  // }

  var numEnabledCycles = 0
  var expectedCount = 0
  var expectedDone = 0

  val maxes = List(96, 100, 200)
  val strides = List(1, 6, 7)

  val saturationVal = 96  // Based on counter width

  step(1)
  reset(1)

  maxes.foreach { max => 
    strides.foreach { stride => 
      numEnabledCycles = 0
      var saturate = 1
      val gap = 0
      var enable = 1

      def testOneStep() = {
        step(1)
        numEnabledCycles += enable
        val count = saturate match {
          case 1 =>
            val count = if (numEnabledCycles * (gap + stride*c.par) < max) {
              (numEnabledCycles * (gap + stride*(c.par))) 
            } else {
              if (max % (gap + stride*(c.par)) == 0) (max - (gap + stride*(c.par))) else max - max % (gap + stride*(c.par))
            }

            count
          case 0 =>
            val numSteps = ( max / (gap + stride*c.par)) // integer type
            val numUntilWrap = if (numSteps * (gap + stride*c.par) == max) numSteps else numSteps+1
            val numWrappedEnabledCycles = numEnabledCycles % numUntilWrap
            val count = if (numWrappedEnabledCycles * (gap + stride*c.par) < max) (numWrappedEnabledCycles * (gap + stride*(c.par))) else (max - max % (gap + stride*(c.par)))

            count
        }
        val done = if ( (count + c.par*stride + gap >= max) & (enable == 1) ) 1 else 0
        // val a = peek(c.io.out(0))
        // val b = peek(c.io.out(1))
        // val cc = peek(c.io.output.done)
        // println("counters at " + a + ", " + b + " and max " + max + " done? " + cc + " expected? " + done + " because " + (count + c.par*stride + gap) + "satmode " + saturate)
        // if (cc != done) println("           ERROR!!!!!!!!!!!!!! \n\n")

        // Check signal values
        (0 until c.par).foreach { i => expect(c.io.output.count(i), count + (i * stride)) }
        expect(c.io.output.done, done)

        expectedCount = count
        expectedDone = done
      }


      poke(c.io.input.max, max)
      poke(c.io.input.gap, gap)
      poke(c.io.input.stride, stride)
      poke(c.io.input.enable, enable)
      poke(c.io.input.saturate, saturate)
      poke(c.io.input.reset, 0)


      for (i <- 1 until 5) {
        testOneStep()
      }

      // Test stall
      enable = 0
      poke(c.io.input.enable, enable)
      for (i <- 1 until 5) {
        testOneStep()
      }

      // Continue
      enable = 1
      poke(c.io.input.enable, enable)
      for (i <- 1 until max) {
        testOneStep()
      }

      // Reset and go again
      numEnabledCycles = 0
      poke(c.io.input.reset, 1)
      step(1)
      poke(c.io.input.reset, 0)
      for (i <- 1 until max+2) {
        testOneStep()
      }

      // Reset and test non-saturating mode
      saturate = 0
      poke(c.io.input.saturate, saturate)
      numEnabledCycles = 0
      poke(c.io.input.reset, 1)
      step(1)
      poke(c.io.input.reset, 0)
      for (i <- 1 until max+2) {
        testOneStep()
      }

      poke(c.io.input.reset, 1)
      step(1)
      reset(1)
      poke(c.io.input.reset, 0)

    }
  }

}

class CounterTester extends ChiselFlatSpec {
  behavior of "Counter"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Counter(3))(c => new CounterTests(c)) should be (true)
    }
  }
}
