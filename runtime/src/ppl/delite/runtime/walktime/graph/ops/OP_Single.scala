package ppl.delite.runtime.walktime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Nov 14, 2010
 * Time: 10:12:48 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

//TODO: given that instances of this class will be created by parsing an input file, is it better to simply pass the type in as a String argument? (rather than having the type parameter)
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
