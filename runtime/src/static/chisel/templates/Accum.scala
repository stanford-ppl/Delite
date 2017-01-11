package templates

import chisel3._

class UIntAccum(val w: Int, val tp: String) extends Module {
  def this(tuple: (Int, String)) = this(tuple._1, tuple._2)

  val io = IO(new Bundle{
    val next = UInt(w.W).asInput
    val enable = Bool().asInput
    val reset = Bool().asInput
    val init = UInt(w.W).asInput
    val output = UInt(w.W).asOutput
  })

  val current = Reg(init = io.init)
  val asyncCurrent = Mux(io.reset, io.init, current)
  val update = tp match { 
    case "add" => asyncCurrent + io.next
    case "max" => Mux(asyncCurrent > io.next, asyncCurrent, io.next)
    case "min" => Mux(asyncCurrent < io.next, asyncCurrent, io.next)
    case _ => asyncCurrent
  }
  current := Mux(io.enable, update, asyncCurrent)

  io.output := asyncCurrent
}
