package ppl.delite.runtime.graph.ops

import java.util.concurrent.locks.ReentrantLock
import ppl.delite.runtime.graph.targets.Targets
import java.util.concurrent.CyclicBarrier

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
  private var bar: CyclicBarrier = null

  def put(res: Any) { result = res }

  def take(): Any = {
    val res = result
    result = null
    res
  }

  def setbarrier(cnt: Int) {
    bar = new CyclicBarrier(cnt, new Runnable() { def run() { signal() }})
  }

  def barrier() { bar.await() }

  def signal() {
   lock.lock
    try {
      notDone = false
      end.signal
    }
    finally {
      lock.unlock
    }
  }

  def await() {
    lock.lock
    try {
      while (notDone) end.await
      notDone = true //reset for re-use
    }
    finally {
      lock.unlock
    }
  }
}

object EOP_Kernel {

  def apply[T](): T = apply(null.asInstanceOf[T])

  def apply[T](result: T): T = {
    EOP_Global.put(result)
    result
  }

}
