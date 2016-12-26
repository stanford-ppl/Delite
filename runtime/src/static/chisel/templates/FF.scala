package templates

import chisel3._

/**
 * FF: Flip-flop with the ability to set enable and init
 * value as IO
 * @param w: Word width
 */

class FFIO(val w: Int) extends Bundle {
    val input = new Bundle {
      val data   = UInt(w.W).asInput
      val init = UInt(w.W).asInput
      val enable = Bool().asInput
      val reset = Bool().asInput // Asynchronous reset
    }
    val output = new Bundle {
      val data  = UInt(w.W).asOutput
    }
}

class FF(val w: Int) extends Module {
  val io = IO(new FFIO(w))

  val ff = Reg(init = io.input.init)
  ff := Mux(io.input.reset, io.input.init, Mux(io.input.enable, io.input.data, ff))
  io.output.data := Mux(io.input.reset, io.input.init, ff)
}

class FFNoInit(val w: Int) extends Module {
  val io = IO(new FFIO(w))

  val ff = Module(new FF(w))
  ff.io.input.data := io.input.data
  ff.io.input.enable := io.input.enable
  ff.io.input.reset := io.input.reset
  ff.io.input.init := UInt(0, w.W)
  io.output.data := ff.io.output.data
}

class FFNoInitNoReset(val w: Int) extends Module {
  val io = IO(new FFIO(w))

  val ff = Module(new FF(w))
  ff.io.input.data := io.input.data
  ff.io.input.enable := io.input.enable
  ff.io.input.reset := Bool(false)
  ff.io.input.init := UInt(0, w.W)
  io.output.data := ff.io.output.data
}

class FFNoReset(val w: Int) extends Module {
  val io = IO(new FFIO(w))

  val ff = Module(new FF(w))
  ff.io.input.data := io.input.data
  ff.io.input.enable := io.input.enable
  ff.io.input.reset := Bool(false)
  ff.io.input.init := io.input.init
  io.output.data := ff.io.output.data
}

class TFF() extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val enable = Bool().asInput
    }
    val output = new Bundle {
      val data = Bool().asOutput      
    }
  })

  val ff = Reg(init = Bool(false))
  ff := Mux(io.input.enable, ~ff, ff)
  io.output.data := ff
}

class SRFF() extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val set = Bool().asInput // Set overrides reset.  Asyn_reset overrides both
      val reset = Bool().asInput
      val asyn_reset = Bool().asInput
    }
    val output = new Bundle {
      val data = Bool().asOutput      
    }
  })

  val ff = Reg(init = Bool(false))
  ff := Mux(io.input.asyn_reset, Bool(false), Mux(io.input.set, 
                                  Bool(true), Mux(io.input.reset, Bool(false), ff)))
  io.output.data := Mux(io.input.asyn_reset, Bool(false), ff)
}


