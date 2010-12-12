package ppl.delite.runtime.executor

import ppl.delite.runtime.codegen.DeliteExecutable
import ppl.delite.runtime.scheduler.StaticSchedule

/**
 * Author: Kevin J. Brown
 * Date: Oct 10, 2010
 * Time: 10:23:53 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * An interface for initializing and submitting work to execution threads
 *
 * @author Kevin J. Brown
 * @param numThreads the number of threads in the pool
 */

class ThreadPool(numThreads: Int) {

  val pool = new Array[ExecutionThread](numThreads)

  def init {
    var i = 0
    while (i < numThreads) {
      val worker = new ExecutionThread
      pool(i) = worker
      val thread = new Thread(worker, "ExecutionThread-"+i) //spawn new machine thread
      thread.setDaemon(true) //to handle shutdown
      thread.start
      i += 1
    }
  }

  def submitOne(id: Int, work: DeliteExecutable) {
    pool(id).queue.put(work)
  }

  /**
   * Puts a static schedule into the appropriate thread queues for execution
   *
   * @param the StaticSchedule to be submitted for execution
   */
  def submitAll(schedule: StaticSchedule) {
    assert(pool.length == schedule.resources.length)
    for (i <- 0 until pool.length) {
      val iter = schedule.resources(i).iterator
      while (iter.hasNext) {
        pool(i).queue.put(iter.next)
      }
    }
  }

}