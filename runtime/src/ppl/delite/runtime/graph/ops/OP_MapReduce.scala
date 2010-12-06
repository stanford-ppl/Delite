package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 8:11:07 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_MapReduce(mapFunc: String, reduceFunc: String, resultType: Map[Targets.Value,String]) extends DeliteOP {

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
    def supportsTarget(target: Targets.Value) = false
    def outputType(target: Targets.Value) = null
    def nested = null
    def cost = 0
    def size = 0
  }

  object Reduce extends DeliteOP {
    def function = reduceFunc

    def isDataParallel = true
    def task = null
    def supportsTarget(target: Targets.Value) = false
    def outputType(target: Targets.Value) = null
    def nested = null
    def cost = 0
    def size = 0
  }

  def supportsTarget(target: Targets.Value) = resultType.contains(target)

  def outputType(target: Targets.Value) = resultType(target)
  override def outputType: String = resultType(Targets.Scala)

  /**
   * Since the semantics of MapReduce are to return a T, all chunks are necessarily complete before the final T can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk: OP_MapReduce = {
    val r = new OP_MapReduce(Map.function, Reduce.function, Targets.unitTypes(resultType))
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def nested = null
  def cost = 0
  def size = 0

}
