// See LICENSE.txt for license details.
package app

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._
import java.io._

class TopModuleTests(c: TopModule, in: String, timeout: Int) extends PeekPokeTester(c) {

  val args = if (in.trim().length() > 0) in.split(" ").toList else List()
  val startTime = System.currentTimeMillis
  var now = System.currentTimeMillis

  step(1)
  reset(1)

  // Enable hw accel
  poke(c.io.top_en, 1)
  (0 until args.length).foreach{ i => poke(c.io.ArgIn.ports(i), args(i).toInt) } // Poke inputs

  // Wait until done or timeout
  var done = peek(c.io.top_done)
  var numCycles = 0
  val stepSize = 100
  while ((done != 1) & (numCycles < timeout)) {
    step(stepSize)
    now = System.currentTimeMillis
    numCycles += stepSize
    done = peek(c.io.top_done)
    if (numCycles % 100 == 0) println(s"[Sim Status] On cycle $numCycles (${(now-startTime)/numCycles}ms/cyc)")
  }
  poke(c.io.top_en, 0)

  val result = peek(c.io.ArgOut.ports(0))

  println(s"Hardware result: $result")

  val pw = new PrintWriter(new File("/tmp/chisel_test_result" ))
  pw.write(s"$result\n$numCycles")
  pw.close
}
