package ppl.delite.runtime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:27:00 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Reduce[T: Manifest](func: String) extends DeliteOP {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  def outputType = manifest[T].toString

  /**
   * Since the semantics of Reduce are to return a T, all chunks are necessarily complete before the final T can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk: OP_Reduce[Unit] = {
    val r = new OP_Reduce[Unit](function)
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def nested = null
  def cost = 0
  def size = 0

}