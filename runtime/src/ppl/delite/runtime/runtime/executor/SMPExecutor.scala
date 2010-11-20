package ppl.delite.runtime.runtime.executor

import ppl.delite.runtime.Config
import ppl.delite.runtime.walktime.scheduler.StaticSchedule

/**
 * Author: Kevin J. Brown
 * Date: Oct 17, 2010
 * Time: 9:55:02 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class SMPExecutor {

  val numThreads = Config.numThreads

  val threadPool = new ThreadPool(numThreads)

  def run(schedule: StaticSchedule) {
    threadPool.init
    println("SMP Executor initialized with " + numThreads + " threads")
    threadPool.submitAll(schedule)
    //TODO: we need a hook to return
    Thread.sleep(5000)
    println("Executor exiting")
  }

}