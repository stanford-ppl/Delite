package ppl.delite.runtime.runtime.executor.gpu

import ppl.delite.runtime.walktime.scheduler.StaticSchedule

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 3:40:38 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * A runtime Executor for a single GPU device
 * This executor creates a pool of streams (analogous to a CPU thread pool)
 */
//TODO: can multiple GPUs be supported through multiple instances of this class?
class GPUExecutor {

  //TODO: how do we choose the appropriate number of streams for the device?
  val numStreams = 1
  val streamPool = new StreamPool(numStreams)

  def run(schedule: StaticSchedule) {
    streamPool.init
    println("GPU Executor initialized")
    streamPool.submitAll(schedule)

    println("GPU Executor exiting")
  }

}