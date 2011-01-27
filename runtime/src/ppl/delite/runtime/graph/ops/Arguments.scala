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

object Arguments extends OP_Executable(Map(Targets.Scala->"Array[java.lang.String]")) {

  /**
   * OP features
   */
  def isDataParallel = false

  def task = "ppl.delite.runtime.graph.ops.ArgsKernel"

  private var _id: String = _
  def id = _id
  private[graph] def id_=(ID: String) { _id = ID }

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