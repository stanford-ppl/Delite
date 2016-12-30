// See LICENSE.txt for license details.
package templates

import chisel3._

import scala.collection.mutable.HashMap

class Sequential(val n: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val enable = Bool().asInput
      val numIter = UInt(32).asInput
      val stageDone = Vec(n, Bool().asInput)
    }
    val output = new Bundle {
      val done = Bool().asOutput
      val stageEnable = Vec(n, Bool().asOutput)
      val rst_en = Bool().asOutput
    }
  })

  // 0: INIT, 1: RESET, 2..2+n-1: stages, n: DONE
  val initState = 0
  val resetState = 1
  val firstState = resetState + 1
  val doneState = firstState + n
  val lastState = doneState - 1

  val stateFF = Module(new FF(32))
  stateFF.io.input.enable := Bool(true) // TODO: Do we need this line?
  stateFF.io.input.init := 0.U
  val state = stateFF.io.output.data

  // Counter for num iterations
  val maxFF = Module(new FF(32))
  maxFF.io.input.enable := io.input.enable
  maxFF.io.input.data := io.input.numIter
  val max = maxFF.io.output.data

  val ctr = Module(new SingleCounter(1))
  ctr.io.input.enable := io.input.enable & io.input.stageDone(lastState-2) // TODO: Is this wrong? It still works...  
  ctr.io.input.reset := (state === doneState.U)
  ctr.io.input.saturate := false.B
  ctr.io.input.max := max
  ctr.io.input.stride := 1.U
  val iter = ctr.io.output.count(0)
  io.output.rst_en := (state === resetState.U)

  when(io.input.enable) {
    when(state === initState.U) {
      stateFF.io.input.data := resetState.U
    }.elsewhen (state === resetState.U) {
      stateFF.io.input.data := Mux(io.input.numIter === 0.U, doneState.U, firstState.U)
    }.elsewhen (state < lastState.U) {

      // // Safe but expensive way
      // val doneStageId = (0 until n).map { i => // Find which stage got done signal
      //   Mux(io.input.stageDone(i), UInt(i+1), 0.U) 
      // }.reduce {_+_}
      // when(state === (doneStageId + 1.U)) {
      //   stateFF.io.input.data := doneStageId + 2.U
      // }.otherwise {
      //   stateFF.io.input.data := state
      // }

      // Less safe but cheap way
      val aStageIsDone = io.input.stageDone.reduce { _ | _ } // TODO: Is it safe to assume children behave properly?
      when(aStageIsDone) {
        stateFF.io.input.data := state + 1.U
      }.otherwise {
        stateFF.io.input.data := state
      }

    }.elsewhen (state === lastState.U) {
      when(io.input.stageDone(lastState-2)) {
        when(ctr.io.output.done) {
          stateFF.io.input.data := doneState.U
        }.otherwise {
          stateFF.io.input.data := firstState.U
        }
      }.otherwise {
        stateFF.io.input.data := state
      }

    }.elsewhen (state === doneState.U) {
      stateFF.io.input.data := initState.U
    }.otherwise {
      stateFF.io.input.data := state
    }
  }.otherwise {
    stateFF.io.input.data := initState.U
  }
//  stateFF.io.input.data := nextStateMux.io.out

  // Output logic
  io.output.done := state === doneState.U
  io.output.stageEnable.zipWithIndex.foreach { case (en, i) => en := (state === UInt(i+2)) }
}
