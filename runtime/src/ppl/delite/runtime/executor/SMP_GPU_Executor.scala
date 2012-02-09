package ppl.delite.runtime.executor

import ppl.delite.runtime.scheduler.StaticSchedule
import ppl.delite.runtime.Config
import ppl.delite.runtime.codegen.{CudaCompile, OpenCLCompile}

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 2:40:47 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * An executor for a single machine consisting of multiple CPUs and GPUs
 * Supports an SMP consisting of an arbitrary number of threads, specified by Config.numThreads
 * Supports an arbitrary number of GPUs, specified by Config.numGPUs
 */
class SMP_GPU_Executor extends Executor {

  val numThreads = Config.numThreads
  val numGPUs = Config.numGPUs

  private val smpExecutor = new SMPExecutor
  private val gpuExecutor = new Array[GPUExecutor](numGPUs)
  for (i <- 0 until numGPUs) gpuExecutor(i) = new GPUExecutor(i)

  def init() {
    smpExecutor.init
    for (i <- 0 until numGPUs) gpuExecutor(i).init
  }

  def run(schedule: StaticSchedule) {
    assert(schedule.resources.length == numThreads + numGPUs)
    smpExecutor.run(new StaticSchedule(schedule.resources.slice(0, numThreads)))
    for (i <- 0 until numGPUs) gpuExecutor(i).run(new StaticSchedule(schedule.resources.slice(numThreads+i, numThreads+i+1)))
  }

  def shutdown() {
    smpExecutor.shutdown
    for (i <- 0 until numGPUs) gpuExecutor(i).shutdown
  }

}
