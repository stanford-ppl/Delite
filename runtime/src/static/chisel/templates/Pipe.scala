package templates

import chisel3._

import scala.collection.mutable.HashMap

// Inner pipe
class Pipe(val ctrDepth : Int) extends Module {

  // States
  val pipeInit = 0
  val pipeReset = 1
  val pipeRun = 2
  val pipeDone = 3
  val pipeSpinWait = 4

  // Module IO
  val io = IO(new Bundle {
    val input = new Bundle {
      val enable = Bool().asInput
      val ctr_done = Bool().asInput
      val ctr_maxIn = Vec(ctrDepth, UInt(32).asInput) // TODO: Deprecate this maxIn/maxOut business if all is well without it
    }
    val output = new Bundle {
      val done = Bool().asOutput
      val ctr_en = Bool().asOutput
      val rst_en = Bool().asOutput
      val ctr_maxOut = Vec(ctrDepth, UInt(32).asOutput)
    }
  })

  val state = Reg(init = pipeInit.U)
  val maxFF = List.tabulate(ctrDepth) { i => Reg(init = 0.U) }

  // Initialize state and maxFF
  val rstCtr = Module(new SingleCounter(1))
  rstCtr.io.input.enable := state === pipeReset.U
  rstCtr.io.input.reset := (state != pipeReset.U)
  rstCtr.io.input.saturate := true.B
  rstCtr.io.input.max := 10.U
  rstCtr.io.input.stride := 1.U

  // Only start the state machine when the enable signal is set
  when (io.input.enable) {
    // Change states
    when( state === pipeInit.U ) {
      io.output.done := false.B
      io.output.ctr_en := false.B
      io.output.rst_en := false.B
      (0 until ctrDepth) foreach { i => maxFF(i) := io.input.ctr_maxIn(i) }
      state := pipeReset.U
    }.elsewhen( state === pipeReset.U ) {
      io.output.rst_en := true.B;
      (0 until ctrDepth) foreach { i => io.output.ctr_maxOut(i) := maxFF(i) }
      when (rstCtr.io.output.done) {
        io.output.rst_en := false.B
        state := pipeRun.U
      }
    }.elsewhen( state === pipeRun.U ) {
      io.output.ctr_en := true.B;
      when (io.input.ctr_done) {
      	(0 until ctrDepth) foreach { i => maxFF(0) := 0.U } // TODO: Why do we reset these instead of leaving them?
        state := pipeDone.U
      }.otherwise {
        state := pipeRun.U
      }
    }.elsewhen( state === pipeDone.U ) {
      io.output.done := true.B
      state := pipeSpinWait.U
    }.elsewhen( state === pipeSpinWait.U ) {
      state := pipeSpinWait.U;
    } 
  }.otherwise {
    io.output.done := false.B
    io.output.ctr_en := false.B
    io.output.rst_en := false.B
    state := pipeInit.U
  }

}

