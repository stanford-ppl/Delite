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
class OP_Single[T: Manifest](kernel: String) extends DeliteOP {

  final def isDataParallel = false

  def task = kernel

  def outputType = manifest[T].toString

  def nested = null
  def cost = 0
  def size = 0

}
