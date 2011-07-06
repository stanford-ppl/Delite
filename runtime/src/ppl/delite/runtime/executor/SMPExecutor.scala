package ppl.delite.runtime.executor

import ppl.delite.runtime.Config
import ppl.delite.runtime.scheduler.StaticSchedule
import ppl.delite.runtime.graph.ops.EOP

/**
 * Author: Kevin J. Brown
 * Date: Oct 17, 2010
 * Time: 9:55:02 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class SMPExecutor extends Executor {

  val numThreads = Config.numThreads

  val threadPool = new ThreadPool(numThreads)

  def init() {
    threadPool.init
  }

  def run(schedule: StaticSchedule) {
    threadPool.submitAll(schedule)
  }

  def shutdown() {
    threadPool.shutdown
  }

}
