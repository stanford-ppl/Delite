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
 * Supports an arbitrary number of Accelerators, specified by Config.numCpp, Config.numCuda, Config.numOpenCL
 */
class SMP_Acc_Executor extends Executor {

  val numThreads = Config.numThreads
  val numAccs = Config.numCpp + Config.numCuda + Config.numOpenCL

  val smpExecutor = new SMPExecutor
  val accExecutor = new Array[AccExecutor](numAccs)
  for (i <- 0 until numAccs) accExecutor(i) = new AccExecutor(i)

  def init() {
    smpExecutor.init
    for (i <- 0 until numAccs) accExecutor(i).init
  }

  def run(schedule: StaticSchedule) {
    assert(schedule.resources.length == numThreads + numAccs)
    smpExecutor.run(new StaticSchedule(schedule.resources.slice(0, numThreads)))
    runAcc(schedule)
  }

  def runAcc(schedule: StaticSchedule) {
    for (i <- 0 until numAccs) accExecutor(i).run(new StaticSchedule(schedule.resources.slice(numThreads+i, numThreads+i+1)))
  }

  def shutdown() {
    smpExecutor.shutdown
    for (i <- 0 until numAccs) accExecutor(i).shutdown
  }

}
