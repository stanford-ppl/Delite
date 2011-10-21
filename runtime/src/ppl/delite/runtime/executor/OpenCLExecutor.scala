package ppl.delite.runtime.executor

import ppl.delite.runtime.scheduler.StaticSchedule
import ppl.delite.runtime.Config

@deprecated("For OpenCL Debug Only. Use SMP+GPU executor instead for normal GPU execution.")
class OpenCLExecutor extends Executor {

  val numThreads = Config.numThreads
  val numGPUs = Config.numGPUs

  assert(Config.useOpenCL)

  private val smpExecutor = new SMPExecutor
  private val gpuExecutor = new Array[GPUExecutor](numGPUs)
  for (i <- 0 until numGPUs) gpuExecutor(i) = new GPUExecutor(i)

  def init() {
    smpExecutor.init
    for (i <- 0 until numGPUs) gpuExecutor(i).init
  }

  def run(schedule: StaticSchedule) {
    assert(schedule.resources.length == numThreads + numGPUs)
    for (i <- 0 until numGPUs) gpuExecutor(i).run(new StaticSchedule(schedule.resources.slice(numThreads+i, numThreads+i+1)))
    Thread.sleep(10000)
    smpExecutor.run(new StaticSchedule(schedule.resources.slice(0, numThreads)))
  }

  def shutdown() {
    smpExecutor.shutdown
    for (i <- 0 until numGPUs) gpuExecutor(i).shutdown
  }

}
