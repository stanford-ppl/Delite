package ppl.delite.runtime.executor

import ppl.delite.io.Config
import ppl.delite.walktime.codegen.DeliteExecutable

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
      i += 1
    }
  }

  def submit(id: Int, work: DeliteExecutable) {
    pool(id).queue.put(work)
  }

}