package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Nov 14, 2010
 * Time: 10:04:13 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Zip(func: String, resultType: Map[Targets.Value,String]) extends DeliteOP {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  def supportsTarget(target: Targets.Value) = resultType.contains(target)

  //TODO: may want output allocation to be a part of the OP => need to remove the below requirement
  assert(resultType == Targets.unitTypes(resultType)) //map must always mutate the elements of a collection and return Unit
  def outputType(target: Targets.Value) = resultType(target)

  /**
   * Since the semantics of the zip are to mutate the elements in a collection all consumer (true) dependency edges already exist in graph
   * Chunking needs to add additional anti-dependency edges for each chunk to ensure all chunks are complete
   * Chunks require same dependency & input lists
   */
  def chunk: OP_Zip = {
    val r = new OP_Zip(function, Targets.unitTypes(resultType))
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.inputList = inputList
    r.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(r)
    for (c <- getConsumers) c.addDependency(r)
    r
  }

  def nested = null
  def cost = 0
  def size = 0
}