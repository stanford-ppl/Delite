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
  val numLoaders = c.io.MemStreams.inPorts.length
  var done = peek(c.io.top_done)
  var numCycles = 0
  val stepSize = 50
  var memRequests = (0 until numLoaders).map{i => 0}
  while ((done != 1) & (numCycles < timeout)) {
    step(stepSize)
    (0 until numLoaders).foreach { i =>
      val req = (peek(c.io.MemStreams.outPorts(i).valid) == 1)
      val size = peek(c.io.MemStreams.outPorts(i).size).toInt
      val base = peek(c.io.MemStreams.outPorts(i).base).toInt
      val addr = peek(c.io.MemStreams.outPorts(i).addr).toInt
      val par = c.io.MemStreams.inPorts(i).data.length
      if (req) {
        for (j <- 0 until size) {
          (0 until par).foreach { k => poke(c.io.MemStreams.inPorts(i).data(k), addr-base+j+k) }  
          poke(c.io.MemStreams.inPorts(i).valid, 1)
          step(1)
        }
        poke(c.io.MemStreams.inPorts(i).valid, 0)
        step(1)
      }
    }
    now = System.currentTimeMillis
    numCycles += stepSize
    done = peek(c.io.top_done)
    if (numCycles % 10000 == 0) println(s"[Sim Status] On cycle $numCycles (${(now-startTime)/numCycles}ms/cyc)")
  }
  poke(c.io.top_en, 0)

  if (numCycles >= timeout) {
    println(s"ERROR: Timeout ($numCycles cycles)")
    expect(c.io.top_en, -999)
  }

  val result = if (c.io.ArgOut.ports.length > 0) { 
    List(peek(c.io.ArgOut.ports(0))) // Arg result
  } else { // Mem result
    List(0)
  }

  println(s"Hardware result: $result")

  val user = System.getProperty("user.name")
  val pw = new PrintWriter(new File(s"/tmp/chisel_test_result_${user}" ))
  pw.write(s"""${result.mkString("\n")}\n$numCycles""")
  pw.close
}
