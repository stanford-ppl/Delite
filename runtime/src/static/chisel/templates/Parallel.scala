// See LICENSE.txt for license details.
package templates

import chisel3._

//A n-stage Parallel controller
class Parallel(val n: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val enable = Bool().asInput
      val stageDone = Vec(n, Bool().asInput)
    }
    val output = new Bundle {
      val done = Bool().asOutput
      val stageEnable = Vec(n, Bool().asOutput)
      val rst_en = Bool().asOutput
    }
  })

  // 0: INIT, 1: RESET, 2 stages enabled, 3: DONE
  // Name the universal states
  val initState = 0
  val runningState = initState + 1
  val doneState = runningState + 1

  // Create FF for holding state
  val stateFF = Module(new FF(2))
  stateFF.io.input.enable := Bool(true)
  stateFF.io.input.init := UInt(0)
  val state = stateFF.io.output.data

  // Create vector of registers for holding stage dones
  val doneFF = List.tabulate(n) { i =>
    val ff = Module(new SRFF())
    ff.io.input.set := io.input.stageDone(i)
    ff.io.input.reset := state === UInt(doneState)
    ff
  }
  val doneMask = doneFF.map { _.io.output.data }

  // // Provide default value for enable and doneClear
  // io.output.stageEnable.foreach { _ := Bool(false) }

  io.output.rst_en := false.B

  // State Machine
  when(io.input.enable) {
    when(state === UInt(initState)) {   // INIT -> RESET
      stateFF.io.input.data := UInt(runningState)
      io.output.rst_en := true.B
    }.elsewhen (state === UInt(runningState)) {  // STEADY
      (0 until n).foreach { i => io.output.stageEnable(i) := ~doneMask(i) }

      val doneTree = doneMask.reduce { _ & _ }
      when(doneTree === 1.U) {
        stateFF.io.input.data := UInt(doneState)
      }.otherwise {
        stateFF.io.input.data := state
      }
    }.elsewhen (state === UInt(doneState)) {  // DONE
      stateFF.io.input.data := UInt(initState)
    }.otherwise {
      stateFF.io.input.data := state
    }
  }.otherwise {
    stateFF.io.input.data := UInt(initState)
    (0 until n).foreach { i => io.output.stageEnable(i) := Bool(false) }
  }

  // Output logic
  io.output.done := state === UInt(doneState)

}









// class ParallelTests(c: Parallel) extends PlasticineTester(c) {
//   val numIter = 5
//   val stageIterCount = List.tabulate(c.numInputs) { i => math.abs(rnd.nextInt) % 10 }
//   println(s"stageIterCount: $stageIterCount")

//   def executeStages(s: List[Int]) {
//     val numCycles = s.map { stageIterCount(_) }
//     var elapsed = 0
//     var done: Int = 0
//     while (done != s.size) {
//       c.io.input.stageDone.foreach { poke(_, 0) }
//       step(1)
//       elapsed += 1
//       for (i <- 0 until s.size) {
//         if (numCycles(i) == elapsed) {
//           println(s"[Stage ${s(i)} Finished execution at $elapsed")
//           poke(c.io.input.stageDone(s(i)), 1)
//           done += 1
//         }
//       }
//     }
//     c.io.input.stageDone.foreach { poke(_, 1) }
//   }

//   def handleStageEnables = {
//     val stageEnables = c.io.output.stageEnable.map { peek(_).toInt }
//     val activeStage = stageEnables.zipWithIndex.filter { _._1 == 1 }.map { _._2 }
//     executeStages(activeStage.toList)
//   }

//   // Start
//   poke(c.io.input.enable, 1)

//   var done = peek(c.io.done).toInt
//   var numCycles = 0
//   while ((done != 1) & (numCycles < 100)) {
//     handleStageEnables
//     done = peek(c.io.done).toInt
//     step(1)
//     numCycles += 1
//   }
// }


// object ParallelTest {

//   def main(args: Array[String]): Unit = {
//     val (appArgs, chiselArgs) = args.splitAt(args.indexOf("end"))

//     val numInputs = 2
//     chiselMainTest(chiselArgs, () => Module(new Parallel(numInputs))) {
//       c => new ParallelTests(c)
//     }
//   }
// }