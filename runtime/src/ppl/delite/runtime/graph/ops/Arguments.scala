package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Nov 30, 2010
 * Time: 3:56:38 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Arguments extends DeliteOP {

  /**
   * OP features
   */
  def isDataParallel = false

  def task = "ppl.delite.runtime.graph.ops.ArgsKernel"

  def supportsTarget(target: Targets.Value): Boolean = {
    if (target == Targets.Scala) true
    else false
  }

  def outputType(target: Targets.Value): String = {
    if (target == Targets.Scala) outputType
    else system.error("Arguments OP does not support targets other than Scala")
  }

  var _id: String = _
  def id = _id
  private[graph] def id_=(ID: String) { _id = ID }

  override def outputType = "Array[String]"

  def nested = null
  def cost = 0
  def size = 0

  /**
   * Parameters Implementation
   */
  var args: Array[String] = _                                                                                                            

}

object ArgsKernel {
  def apply(): Array[String] = Arguments.args
}