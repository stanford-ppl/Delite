package ppl.delite.runtime.graph.ops

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

  def outputType = "Array[String]"

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