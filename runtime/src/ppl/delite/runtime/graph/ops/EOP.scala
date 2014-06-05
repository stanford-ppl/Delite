package ppl.delite.runtime.graph.ops

import java.util.concurrent.locks.ReentrantLock
import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Nov 29, 2010
 * Time: 11:21:14 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This is a special singleton OP that signifies the end of the application
 * This OP should always be inserted by the scheduler such that it is the last to run (depends on the "result" node of the task graph
 * Execution of the kernel will shut down the Delite Runtime
 */
class EOP(val id: String, var outputTypesMap: Map[Targets.Value,Map[String,String]], val result: (String,String)) extends OP_Executable {

  /**
   * OP features
   */

  def isDataParallel = false

  def task = "ppl.delite.runtime.graph.ops.EOP_Kernel"

  def cost = 0
  def size = 0

}

object EOP_Global {

  /**
   * EOP implementation
   */
  private val lock = new ReentrantLock
  private val end = lock.newCondition
  private var notDone: Boolean = true
  private var result: Any = null

  def put(res: Any) {
    lock.lock
    try {
      notDone = false
      result = res
      end.signal
    }
    finally {
      lock.unlock
    }
  }

  def take(): Any = {
    var res: Any = null
    lock.lock
    try {
      while (notDone) end.await
      notDone = true //reset for re-use
      res = result
      result = null
    }
    finally {
      lock.unlock
    }
    res
  }  
}

object EOP_Kernel {

  def apply[T](): T = apply(null.asInstanceOf[T])

  def apply[T](result: T): T = {
    EOP_Global.put(result)
    result
  }

}
