// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * SingleCounter test harness
 */

class SingleCounterTests(c: SingleCounter) extends PeekPokeTester(c) {

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
  val starts = List(0, 5)

  step(1)
  reset(1)

  maxes.foreach { max => 
    strides.foreach { stride => 
      starts.foreach { start =>
        numEnabledCycles = 0
        var saturate = 1
        val gap = 0
        var enable = 1

        def testOneStep() = {
          step(1)
          numEnabledCycles += enable
          val count = saturate match {
            case 1 =>
              val count = if (start + numEnabledCycles * (gap + stride*c.par) < max) {
                (start + numEnabledCycles * (gap + stride*(c.par))) 
              } else {
                if ((max-start) % (gap + stride*(c.par)) == 0) (max - (gap + stride*(c.par))) else max - (max-start) % (gap + stride*(c.par))
              }

              count
            case 0 =>
              val numSteps = ( (max-start) / (gap + stride*c.par)) // integer type
              val numUntilWrap = if (numSteps * (gap + stride*c.par) == (max-start)) numSteps else numSteps+1
              val numWrappedEnabledCycles = numEnabledCycles % numUntilWrap
              val count = if (start + numWrappedEnabledCycles * (gap + stride*c.par) < max) (start + numWrappedEnabledCycles * (gap + stride*(c.par))) else (max - max % (gap + stride*(c.par)))

              count
          }
          val done = if ( (count + c.par*stride + gap >= max) & (enable == 1) ) 1 else 0
          // val a = peek(c.io.output.count(0))
          // val b = peek(c.io.output.count(1))
          // val cc = peek(c.io.output.done)
          // println(s"SingleCounters at $a, $b, (want $count), max $max done? $cc expected? $done because ${(count + c.par*stride + gap)} satmode $saturate")
          // if (cc != done) println("           ERROR!!!!!!!!!!!!!! \n\n")

          // Check signal values
          (0 until c.par).foreach { i => expect(c.io.output.count(i), count + (i * stride)) }
          expect(c.io.output.done, done)

          expectedCount = count
          expectedDone = done
        }

        poke(c.io.input.enable, 0)
        poke(c.io.input.start, start)
        step(5)
        poke(c.io.input.reset, 1)
        step(1)
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

        poke(c.io.input.enable, 0)
        poke(c.io.input.reset, 1)
        step(1)
        reset(1)
        poke(c.io.input.reset, 0)

      }
    }
  }

}

class CounterTests(c: Counter) extends PeekPokeTester(c) {

  // Test triple nested counter
  val depth = 3
  var numEnabledCycles = 0
  // var expectedCounts = List(0,0,0)
  // var expectedDones = List(0,0,0)

  val gap = List(0,0,0)
  val maxes = List(List(8,12,8), List(4,8,16), List(8,20,200))
  val strides = List(List(3,3,3),List(1,1,1), List(3, 6, 5))
  val start = List(0,0,0) // TODO: Test new starts
  var enable = 1
  var saturate = 1

  step(1)
  reset(1)

  maxes.foreach { max => 
  strides.foreach { stride => 
    val alignedMax = max.zip(stride).zip(c.par).map {case ((m,s),p) => 
      if (m % (s*p) == 0) m else m - (m % (s*p)) + (s*p) 
    }
    numEnabledCycles = 0
    val totalTicks = alignedMax.reduce{_*_} / stride.reduce{_*_} // TODO: Incorporate stride/par/starts
    val stepSizes = c.par.zipWithIndex.map{case (p,i) => p*stride(i) + gap(i)}

    def testOneStep() = {
        step(1)
        if (enable == 1) numEnabledCycles += 1
        val baseCnts = (0 until depth).map { i => 
          peek(c.io.output.countBases(i))
        }
        val cksum = baseCnts.drop(1).zipWithIndex.map{ case (cnt, i) => 
          max.take(i+1).reduce{_*_} * cnt
        }.reduce{_+_} + baseCnts(0)
        val expectedCksum = numEnabledCycles // TODO: Incorporate stride/par/starts
        val done = if (numEnabledCycles == max.reduce{_*_}) 1 else 0

        c.par.zipWithIndex.foreach{ case (p,i) => 
          val ticksToInc = (alignedMax.take(i+1).reduce{_*_} * stepSizes(i)) / (alignedMax(i) * stepSizes.take(i+1).reduce{_*_})
          val period = ticksToInc*alignedMax(i) / stepSizes(i)
          val increments = (numEnabledCycles) / ticksToInc
          val base = if (saturate == 1) {
            if (numEnabledCycles >= totalTicks) {
              alignedMax(i) - p*stride(i)
            } else {
              (increments * stepSizes(i)) % alignedMax(i)
            }
          } else {
            increments % alignedMax(i) // TODO: Not sure if this is correct, only testing saturating ctrs now
          }
          val ctrAddr = c.par.take(i+1).reduce{_+_} - c.par(i)
          (0 until p).foreach{ k => 
//             val test = peek(c.io.output.counts(ctrAddr+k))
//             if (test != base + k*stride(i)) {
//               println(s"""Step ${numEnabledCycles}: (checking ctr${i}.${k} @ ${ctrAddr+k} (hw: ${test} =? ${base+k*stride(i)})
//   tic each ${ticksToInc} from ${alignedMax.take(i+1).reduce{_*_}} / ${alignedMax(i)}), 
//     increments = ${increments}
//       base = ${base} (incs % ${alignedMax(i)})
// """)
//             }
            expect(c.io.output.counts(ctrAddr+k), base + k*stride(i))
          }
        }
      }

      enable = 0
      poke(c.io.input.enable, enable)
      c.io.input.starts.zip(start).foreach{ case (wire, value) => poke(wire,value) }
      step(5)
      c.io.input.maxes.zip(max).foreach{ case (wire, value) => poke(wire,value) }
      c.io.input.strides.zip(stride).foreach{ case (wire, value) => poke(wire,value) }
      poke(c.io.input.reset, 1)
      step(1)
      enable = 1
      poke(c.io.input.enable, enable)
      poke(c.io.input.saturate, saturate)
      poke(c.io.input.reset, 0)

      for (i <- 0 until (totalTicks*1.1).toInt) {
        testOneStep
      }

      enable = 0
      poke(c.io.input.enable, enable)
      poke(c.io.input.reset, 1)
      step(1)
      reset(1)
      poke(c.io.input.reset, 0)

    }
  }


}


class SingleCounterTester extends ChiselFlatSpec {
  behavior of "SingleCounter"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new SingleCounter(3))(c => new SingleCounterTests(c)) should be (true)
    }
  }
}

class CounterTester extends ChiselFlatSpec {
  behavior of "Counter"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Counter(List(1,1,1)))(c => new CounterTests(c)) should be (true)
    }
  }
}
