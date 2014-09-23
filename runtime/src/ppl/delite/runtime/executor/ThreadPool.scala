package ppl.delite.runtime.executor

import ppl.delite.runtime.scheduler.StaticSchedule

/**
 * An interface for initializing and submitting work to execution threads
 */

class ThreadPool(numThreads: Int, executor: Int => ExecutionThread) {

  private val pool = new Array[ExecutionThread](numThreads)
  private val threads = new Array[Thread](numThreads)

  def init() {
    for (i <- 0 until numThreads) {
      val worker = executor(i)
      pool(i) = worker
      val thread = new Thread(worker, worker.getClass.getSimpleName+i) //spawn new machine thread
      threads(i) = thread
      thread.start
    }
  }

  def shutdown() {
    for (i <- 0 until threads.length) {
      //println("shutting down executor thread "+i)
      threads(i).interrupt()
    }
  }

  def submitOne(location: Int, item: DeliteExecutable) {
    pool(location).queue.put(item)
  }

  /**
   * Puts a static schedule into the appropriate thread queues for execution
   *
   * @param the StaticSchedule to be submitted for execution
   */
  def submitAll(schedule: StaticSchedule) {
    assert(pool.length >= schedule.resources.length)
    for (i <- 0 until schedule.resources.length) {
      for (exec <- schedule.resources(i)) {
        pool(i).queue.put(exec)
      }
    }
  }

}
