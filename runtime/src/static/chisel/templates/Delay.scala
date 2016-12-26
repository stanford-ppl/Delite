// See LICENSE.txt for license details.
package templates

import chisel3._

class Delay(val length: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val data      = UInt(32.W).asInput
    }
    val output = new Bundle {
      val data = UInt(32.W).asOutput
    }
  })

  val regs = (0 until length-1).map { i => Reg(init = UInt(0))}
  regs(0) := io.input.data
  (length-2 to 1 by -1).map { i =>
    regs(i) := regs(i-1)
  }
  io.output.data := regs(length-2)
}

// class DelayReg(val w: Int) extends Module {
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

//   // Instantiate Delay
//   val Delay = Module(new Delay(w))
//   Delay.io.data.max := max
//   Delay.io.data.stride := stride
//   Delay.io.control.enable := enable
//   Delay.io.control.input.reset := rst
//   Delay.io.control.enable := enable
//   Delay.io.control.saturate := saturate

//   // Register outputs
//   val outReg = Module(new FF(w))
//   outReg.io.control.enable := Bool(true)
//   outReg.io.data.in := Delay.io.data.output.count
//   io.data.output.count := outReg.io.data.output.count
//   val doneReg = Module(new FF(1))
//   doneReg.io.control.enable := Bool(true)
//   doneReg.io.data.in := Delay.io.control.done
//   io.control.done := doneReg.io.data.output.count
// }

