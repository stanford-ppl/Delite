package templates

import chisel3._
import chisel3.util



class FIFO(val p: Int, val depth: Int) extends Module {
  def this(tuple: (Int, Int)) = this(tuple._1, tuple._2)

  val io = IO( new Bundle {
    val in = Vec(p, UInt(32.W).asInput)
    val out = Vec(p, UInt(32.W).asOutput)
    val push = Bool().asInput
    val pop = Bool().asInput
    val debug = new Bundle {
      val overflow = Bool().asOutput
    }
  })

  // Create physical mems
  val m = (0 until p).map{ i => Module(new Mem1D(depth/p))}

  // Create head and reader counters
  val writer = Module(new SingleCounter(1))
  val reader = Module(new SingleCounter(1))
  writer.io.input.max := (depth/p).U
  writer.io.input.enable := io.push
  writer.io.input.stride := 1.U
  reader.io.input.max := (depth/p).U
  reader.io.input.enable := io.pop
  reader.io.input.stride := 1.U

  // Connect pusher
  m.zip(io.in).foreach { case (mem, data) => 
    mem.io.w.addr := writer.io.output.count(0)
    mem.io.w.data := data
    mem.io.w.en := io.push
  }

  // Connect popper
  m.zip(io.out).foreach { case (mem, data) => 
    mem.io.r.addr := reader.io.output.count(0)
    mem.io.r.en := io.pop
    data := mem.io.output.data
  }

  // Debugger
  val ov = Module(new SRFF())
  val overflowing = (writer.io.output.count(0) === reader.io.output.count(0)) & io.pop
  val fixed_overflow = (writer.io.output.count(0) + 1.U === reader.io.output.count(0)) & io.push // TODO: Need to consider wrap
  ov.io.input.set := overflowing
  ov.io.input.reset := fixed_overflow
  io.debug.overflow := ov.io.output.data

}

