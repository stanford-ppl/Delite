package ppl.delite.runtime
package profiler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import java.lang.management.ManagementFactory


//front-facing interface to activate all profiling tools
object Profiling {

  private var globalStartNanos = 0L
  private var jvmUpTime = 0L

  def init(graph: DeliteTaskGraph) {
    Profiler.init(graph)
    val totalResources = Config.numThreads + Config.numCpp + Config.numCuda + Config.numOpenCL
    PerformanceTimer.initializeStats(totalResources)
    MemoryProfiler.initializeStats(totalResources)
  }

  def startRun() {
    PerformanceTimer.clearAll()
    globalStartNanos = System.nanoTime()
    jvmUpTime = ManagementFactory.getRuntimeMXBean().getUptime()
    PerformanceTimer.start("all", false)
  }

  def endRun() {
    PerformanceTimer.stop("all", false)
    PerformanceTimer.printStatsForNonKernelComps()
    if (Config.dumpProfile) Profiler.dumpProfile(globalStartNanos, jvmUpTime)  
    if (Config.dumpStats) PerformanceTimer.dumpStats()   
  }

}
