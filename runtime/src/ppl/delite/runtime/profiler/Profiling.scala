package ppl.delite.runtime
package profiler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import java.lang.management.ManagementFactory
import tools.nsc.io.Path

//front-facing interface to activate all profiling tools
object Profiling {

  private var globalStartNanos = 0L
  private var jvmUpTime = 0L

  def init(graph: DeliteTaskGraph) {
    Profiler.init(graph)
    val totalResources = Config.numThreads + Config.numCpp + Config.numCuda + Config.numOpenCL
    Path(Config.profileOutputDirectory).createDirectory()
    PerformanceTimer.initializeStats(totalResources)
    MemoryProfiler.initializeStats(Config.numThreads, Config.numCpp, Config.numCuda, Config.numOpenCL)
  }

  def startRun() {
    PerformanceTimer.recordAppStartTimeStats()

    PerformanceTimer.clearAll()
    MemoryProfiler.clearAll()

    globalStartNanos = System.nanoTime()
    jvmUpTime = ManagementFactory.getRuntimeMXBean().getUptime()
    
    if (Config.dumpProfile) SamplerThread.start()
  }

  def endRun() {
    if (Config.dumpProfile) SamplerThread.stop()
	PerformanceTimer.stop()
    PerformanceTimer.printStatsForNonKernelComps()
    if (Config.dumpProfile) PerformanceTimer.stop()  
    if (Config.dumpStats) PerformanceTimer.dumpStats()   

	if (Config.dumpProfile) {
		val t0 = System.nanoTime()
		PostProcessor.postProcessProfileData(globalStartNanos, Config.degFilePath)
		val t1 = System.nanoTime()
		Predef.println("[TIME] postProcessProfileData: " + ((t1 - t0)/1000000) + " ms")
	}
  }
}
