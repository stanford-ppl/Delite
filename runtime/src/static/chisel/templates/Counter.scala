// See LICENSE.txt for license details.
package templates

import chisel3._

/**
 * Counter: 1-dimensional counter. Counts upto 'max', each time incrementing
 * by 'stride', beginning at zero.
 * @param w: Word width
 */
class Counter(val par: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val max      = UInt(32.W).asInput
      val stride   = UInt(32.W).asInput
      val gap      = UInt(32).asInput
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
  val init = UInt(0)
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

// class CounterReg(val w: Int) extends Module {
//   val io = new Bundle {
//     val data = new Bundle {
//       val max      = UInt(INPUT,  w)
//       val stride   = UInt(INPUT,  w)
//       val output.count      = UInt(OUTPUT, w)
//     }
//     val control = new Bundle {
//       vinput.al reset = Bool(INPUT)
//       val enable = Bool(INPUT)
//       val saturate = Bool(INPUT)
//       val done   = Bool(OUTPUT)
//     }
//   }

//   // Register the inputs
//   val maxReg = Module(new FF(w))
//   maxReg.io.control.enable := Bool(true)
//   maxReg.io.data.in := io.data.max
//   val max = maxReg.io.data.output.count

//   val strideReg = Module(new FF(w))
//   strideReg.io.control.enable := Bool(true)
//   strideReg.io.data.in := io.data.stride
//   val stride = strideReg.io.data.output.count

//   val rstReg = Module(new FF(1))
//   rstReg.io.control.enable := Bool(true)
//   rstReg.io.data.in := io.control.input.reset
//   val rst = rstReg.io.data.output.count

//   val enableReg = Module(new FF(1))
//   enableReg.io.control.enable := Bool(true)
//   enableReg.io.data.in := io.control.enable
//   val enable = enableReg.io.data.output.count

//   val saturateReg = Module(new FF(1))
//   saturateReg.io.control.enable := Bool(true)
//   saturateReg.io.data.in := io.control.saturate
//   val saturate = saturateReg.io.data.output.count

//   // Instantiate counter
//   val counter = Module(new Counter(w))
//   counter.io.data.max := max
//   counter.io.data.stride := stride
//   counter.io.control.enable := enable
//   counter.io.control.input.reset := rst
//   counter.io.control.enable := enable
//   counter.io.control.saturate := saturate

//   // Register outputs
//   val outReg = Module(new FF(w))
//   outReg.io.control.enable := Bool(true)
//   outReg.io.data.in := counter.io.data.output.count
//   io.data.output.count := outReg.io.data.output.count
//   val doneReg = Module(new FF(1))
//   doneReg.io.control.enable := Bool(true)
//   doneReg.io.data.in := counter.io.control.done
//   io.control.done := doneReg.io.data.output.count
// }

