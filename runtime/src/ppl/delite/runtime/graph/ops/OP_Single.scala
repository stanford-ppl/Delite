package ppl.delite.runtime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Nov 14, 2010
 * Time: 10:12:48 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Single extends DeliteOP {

  var kernelId: String = _
  var scalaResultType: String = _

  final def isDataParallel = false

  def task = kernelId

  def outputType = scalaResultType

  def nested = null
  def cost = 0
  def size = 0

}
