// See LICENSE.txt for license details.
package templates

import chisel3._


/**
 * NBufCtr: 1-dimensional counter. Basically a cheap, wrapping counter because  
             chisel is retarted and optimizes away a Vec(1) to a single val,
             but still forces you to index the thing and hence only gets the
             first bit
 */
class NBufCtr() extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val start    = UInt(32.W).asInput // TODO: Currently resets to "start" but wraps to 0, is this normal behavior?
      val max      = UInt(32.W).asInput
      val countUp  = Bool().asInput
      val enable = Bool().asInput
    }
    val output = new Bundle {
      val count      = UInt(32.W).asOutput
    }
  })

  val cnt = Reg(init = io.input.start)

  val nextCntDown = Mux(io.input.enable, Mux(cnt === 0.U, io.input.max-1.U, cnt-1.U), cnt)
  val nextCntUp = Mux(io.input.enable, Mux(cnt + 1.U === io.input.max, 0.U, cnt+1.U), cnt)
  cnt := Mux(io.input.countUp, nextCntUp, nextCntDown)

  io.output.count := cnt
}

/**
 * RedxnCtr: 1-dimensional counter. Basically a cheap, wrapping for reductions
 */
class RedxnCtr() extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val max      = UInt(32.W).asInput
      val enable = Bool().asInput
    }
    val output = new Bundle {
      val done      = Bool().asOutput
    }
  })

  val cnt = Reg(init = 0.U)

  val nextCntUp = Mux(io.input.enable, Mux(cnt + 1.U === io.input.max, 0.U, cnt+1.U), cnt)
  cnt := nextCntUp

  io.output.done := cnt === io.input.max
}

/**
 * SingleCounter: 1-dimensional counter. Counts upto 'max', each time incrementing
 * by 'stride', beginning at zero.
 * @param w: Word width
 */
class SingleCounter(val par: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val start    = UInt(32.W).asInput // TODO: Currently resets to "start" but wraps to 0, is this normal behavior?
      val max      = UInt(32.W).asInput
      val stride   = UInt(32.W).asInput
      val gap      = UInt(32).asInput
      // val wrap     = Bool().asInput // TODO: This should let 
      //                                   user specify (8 by 3) ctr to go
      //                                   0,3,6 (wrap) 1,4,7 (wrap) 2,5...
      //                                   instead of default
      //                                   0,3,6 (wrap) 0,3,6 (wrap) 0,3...
      val reset  = Bool().asInput
      val enable = Bool().asInput
      val saturate = Bool().asInput
    }
    val output = new Bundle {
      val count      = Vec(par, UInt(32.W).asOutput)
      val done   = Bool().asOutput
      val extendedDone = Bool().asOutput
      val saturated = Bool().asOutput
    }
  })

  val base = Module(new FF(32))
  val init = io.input.start
  base.io.input.init := init
  base.io.input.enable := io.input.reset | io.input.enable

  val count = base.io.output.data
  val newval = count + (io.input.stride * UInt(par)) + io.input.gap
  val isMax = newval >= io.input.max
  val wasMax = Reg(next = isMax, init = Bool(false))
  val wasEnabled = Reg(next = io.input.enable, init = Bool(false))
  val next = Mux(isMax, Mux(io.input.saturate, count, init), newval)
  base.io.input.data := Mux(io.input.reset, init, next)

  (0 until par).foreach { i => io.output.count(i) := count + UInt(i)*io.input.stride }
  io.output.done := io.input.enable & isMax
  io.output.saturated := io.input.saturate & isMax
  io.output.extendedDone := (io.input.enable | wasEnabled) & (isMax | wasMax)
}


/*
     innermost    middle   outermost
      |     |    |     |    |     |
      |     |    |     |    |     |
      |_____|    |_____|    |_____|
      _| | |_    __| |    _____|
     |   |   |  |    |   |   
count(0) 1   2  3    4   5

*/

/**
 * Counter: n-depth counter. Counts up to each max. Lists go from
            innermost (fastest) to outermost (slowest) counter.
 * @param w: Word width
 */
class Counter(val par: List[Int]) extends Module {
  val depth = par.length
  val numWires = par.reduce{_+_}

  val io = IO(new Bundle {
    val input = new Bundle {
      val starts    = Vec(depth, UInt(32.W).asInput)
      val maxes      = Vec(depth, UInt(32.W).asInput)
      val strides   = Vec(depth, UInt(32.W).asInput)
      val gaps      = Vec(depth, UInt(32).asInput)
      val reset  = Bool().asInput
      val enable = Bool().asInput
      val saturate = Bool().asInput
    }
    val output = new Bundle {
      val counts      = Vec(numWires, UInt(32.W).asOutput) 
      // val countBases  = Vec(depth, UInt(32.W).asOutput) // For debugging the base of each ctr
      val done   = Bool().asOutput
      val extendedDone   = Bool().asOutput // Tool for ensuring done signal is stable for one rising edge
      val saturated = Bool().asOutput
    }
  })

  // Create counters
  val ctrs = (0 until depth).map{ i => Module(new SingleCounter(par(i))) }

  // Wire up the easy inputs from IO
  ctrs.zipWithIndex.foreach { case (ctr, i) =>
    ctr.io.input.start := io.input.starts(i)
    ctr.io.input.max := io.input.maxes(i)
    ctr.io.input.stride := io.input.strides(i)
    ctr.io.input.gap := io.input.gaps(i)
    ctr.io.input.reset := io.input.reset
  }

  // Wire up the enables between ctrs
  ctrs(0).io.input.enable := io.input.enable
  (1 until depth).foreach { i =>
    ctrs(i).io.input.enable := ctrs(i-1).io.output.done & io.input.enable
  }

  // Wire up the saturates between ctrs
  ctrs(depth-1).io.input.saturate := io.input.saturate
  (0 until depth-1).foreach { i =>
    ctrs(i).io.input.saturate := io.input.saturate & ctrs.drop(i+1).map{ ctr => ctr.io.output.saturated }.reduce{_&_}
  }

  // Wire up the outputs
  par.zipWithIndex.foreach { case (p, i) => 
    val addr = par.take(i+1).reduce{_+_} - par(i) // i+1 to avoid reducing empty list
    (0 until p).foreach { k => io.output.counts(addr+k) := ctrs(i).io.output.count(k) }
  }

  // // Wire up countBases for easy debugging
  // ctrs.zipWithIndex.map { case (ctr,i) => 
  //   io.output.countBases(i) := ctr.io.output.count(0)
  // }

  // Wire up the done, saturated, and extendedDone signals
  val isDone = ctrs.map{_.io.output.done}.reduce{_&_}
  val wasDone = Reg(next = isDone, init = Bool(false))
  val isSaturated = ctrs.map{_.io.output.saturated}.reduce{_&_}
  val wasWasDone = Reg(next = wasDone, init = Bool(false))
  io.output.done := io.input.enable & isDone & ~wasDone
  io.output.extendedDone := io.input.enable & isDone & ~wasWasDone
  io.output.saturated := io.input.saturate & isSaturated

}


