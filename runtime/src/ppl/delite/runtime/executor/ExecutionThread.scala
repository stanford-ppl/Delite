package ppl.delite.runtime.executor

import java.util.concurrent.LinkedBlockingQueue
import ppl.delite.io.Config
import ppl.delite.walktime.codegen.DeliteExecutable

/**
 * Author: Kevin J. Brown
 * Date: Oct 10, 2010
 * Time: 10:24:14 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 *
 * A Runnable that represents the work of an execution thread for the CPU
 *
 * @author Kevin J. Brown
 */

class ExecutionThread extends Runnable {

  // the work queue for this ExecutionThread
  // synchronization is handled by the queue implementation
  private[executor] val queue = new LinkedBlockingQueue[DeliteExecutable](Config.queueSize) //work queue


  //this infinite loop should be terminated by making the thread executing this Runnable a daemon
  def run {
    while(true) {
      val work = queue.take //blocking
      executeWork(work)
    }
  }

  // how to execute work
  private def executeWork(work: DeliteExecutable) = work.run

}