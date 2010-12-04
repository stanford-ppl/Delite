package ppl.delite.runtime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 8:11:07 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_MapReduce(mapFunc: String, reduceFunc: String, resultType: String) extends DeliteOP {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  object Map extends DeliteOP{
    def function = mapFunc

    def isDataParallel = true
    def task = null
    def outputType = null
    def nested = null
    def cost = 0
    def size = 0
  }

  object Reduce extends DeliteOP {
    def function = reduceFunc

    def isDataParallel = true
    def task = null
    def outputType = null
    def nested = null
    def cost = 0
    def size = 0
  }

  def outputType = resultType

  /**
   * Since the semantics of Reduce are to return a T, all chunks are necessarily complete before the final T can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk: OP_MapReduce = {
    val r = new OP_MapReduce(Map.function, Reduce.function, "Unit")
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def nested = null
  def cost = 0
  def size = 0

}
