// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import utils.TemplateRunner

object Launcher {
  val templates = Map(
      "FF" -> { (backendName: String) =>
        Driver(() => new FF(32), "verilator") {
          (c) => new FFTests(c)
        }
      },
      "FFNoInit" -> { (backendName: String) =>
        Driver(() => new FFNoInit(32), "verilator") {
          (c) => new FFNoInitTests(c)
        }
      },
      "FFNoInitNoReset" -> { (backendName: String) =>
        Driver(() => new FFNoInit(32), "verilator") {
          (c) => new FFNoInitTests(c)
        }
      },
      "FFNoReset" -> { (backendName: String) =>
        Driver(() => new FFNoInit(32), "verilator") {
          (c) => new FFNoInitTests(c)
        }
      },
      "TFF" -> { (backendName: String) =>
        Driver(() => new TFF(), "verilator") {
          (c) => new TFFTests(c)
        }
      },
      "SRFF" -> { (backendName: String) =>
        Driver(() => new SRFF(), "verilator") {
          (c) => new SRFFTests(c)
        }
      },
      "SingleCounter" -> { (backendName: String) =>
        Driver(() => new SingleCounter(3), "verilator") {
          (c) => new SingleCounterTests(c)
        }
      },
      "Counter" -> { (backendName: String) =>
        Driver(() => new Counter(List(2,2,2)), "verilator") {
          (c) => new CounterTests(c)
        }
      },
      "Sequential" -> { (backendName: String) =>
        Driver(() => new Sequential(10), "verilator") {
          (c) => new SequentialTests(c)
        }
      },
      "Metapipe" -> { (backendName: String) =>
        Driver(() => new Metapipe(5), "verilator") {
          (c) => new MetapipeTests(c)
        }
      },
      "Delay" -> { (backendName: String) =>
        Driver(() => new Delay(10), "verilator") {
          (c) => new DelayTests(c)
        }
      },
      "Mem1D" -> { (backendName: String) =>
        Driver(() => new Mem1D(1024), "verilator") {
          (c) => new Mem1DTests(c)
        }
      },
      "MemND" -> { (backendName: String) =>
        Driver(() => new MemND(List(4,8)), "verilator") {
          (c) => new MemNDTests(c)
        }
      },
      "SRAM" -> { (backendName: String) =>
        Driver(() => new SRAM(List(16,16), 1, 32, 
                              List(1,2), List(1,1), 1, 1,
                              2, 2, "strided"), "verilator") {
          (c) => new SRAMTests(c)
        }
      },
      "Pipe" -> { (backendName: String) =>
        Driver(() => new Pipe(2), "verilator") {
          (c) => new PipeTests(c)
        }
      },
      "Parallel" -> { (backendName: String) =>
        Driver(() => new Parallel(3), "verilator") {
          (c) => new ParallelTests(c)
        }
      }
  )
  def main(args: Array[String]): Unit = {
    TemplateRunner(templates, args)
  }
}

