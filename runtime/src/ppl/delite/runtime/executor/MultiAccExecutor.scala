package ppl.delite.runtime.executor

import ppl.delite.runtime.scheduler.StaticSchedule
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets

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
 * Supports an SMP consisting of an arbitrary number of threads, specified by Config.numThreads, Config.numCpp
 * Supports an arbitrary number of accelerators, specified by Config.numCuda, Config.numOpenCL
 */
class MultiAccExecutor extends Executor {

  private val threadPools = Map(Targets.Scala -> new ThreadPool(Config.numThreads, i => new ExecutionThread()), 
                                Targets.Cpp -> new ThreadPool(Config.numCpp, i => new CppExecutionThread(i, Config.numCpp)), 
                                Targets.Cuda -> new ThreadPool(Config.numCuda, i => new CudaExecutionThread(i, Config.numCuda)), 
                                Targets.OpenCL -> new ThreadPool(Config.numOpenCL, i => new OpenCLExecutionThread(i, Config.numOpenCL))
  )

  def init() {
    for ((t,pool) <- threadPools) pool.init()
  }

  def run(schedule: StaticSchedule) {
    for ((t, pool) <- threadPools) pool.submitAll(schedule.slice(Targets.resourceIDs(t)))
  }

  def runOne(location: Int, item: DeliteExecutable) {
    threadPools(Targets.getByLocation(location)).submitOne(Targets.getRelativeLocation(location), item)
  }

  def shutdown() {
    for ((t,pool) <- threadPools) pool.shutdown()
  }

}
