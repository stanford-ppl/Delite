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

  def task = if (scheduledOn(Targets.Scala)) "ppl.delite.runtime.graph.ops.EOP_Kernel"
             else if (scheduledOn(Targets.Cpp)) "cppDeepCopy"
             else throw new RuntimeException("Unsupported target for EOP")

  def cost = 0
  def size = 0

}

object EOP_Global {

  private var result: Any = null
  private var barrier: CyclicBarrier = null

  def put(res: Any) { result = res }

  def take(): Any = {
    val res = result
    result = null
    res
  }

  def initializeBarrier(count: Int) {
    barrier = new CyclicBarrier(count)
  }

  def awaitBarrier() { barrier.await() }

}

object EOP_Kernel {

  def apply[T](): T = apply(null)

  def apply[T](resourceInfo: generated.scala.ResourceInfo): T = apply(resourceInfo, null.asInstanceOf[T])

  def apply[T](resourceInfo: generated.scala.ResourceInfo, result: T): T = {
    EOP_Global.put(result)
    result
  }

}
