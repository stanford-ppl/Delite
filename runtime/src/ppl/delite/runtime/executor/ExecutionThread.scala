package ppl.delite.runtime.executor

import java.util.concurrent.LinkedBlockingQueue
import ppl.delite.runtime.Config
import ppl.delite.runtime.codegen.DeliteExecutable

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

  private[executor] var continue: Boolean = true

  //this loop should be terminated by executing a special shutdown Executable
  def run {
    while(continue) {
      val work = queue.take //blocking
      executeWork(work)
    }
  }

  // how to execute work
  private def executeWork(work: DeliteExecutable) = work.run

}
