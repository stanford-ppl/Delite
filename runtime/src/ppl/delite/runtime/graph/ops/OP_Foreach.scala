package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:26:52 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Foreach(val id: String, func: String, resultType: Map[Targets.Value,String]) extends DeliteOP {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  def supportsTarget(target: Targets.Value) = resultType.contains(target)

  assert(resultType == Targets.unitTypes(resultType)) //foreach must always mutate the elements of a collection and return Unit
  def outputType(target: Targets.Value) = resultType(target)

  /**
   * Since the semantics of the foreach are to mutate the elements in a collection all consumer (true) dependency edges already exist in graph
   * Chunking needs to add additional anti-dependency edges for each chunk to ensure all chunks are complete
   * Chunks require same dependency & input lists
   */
  def chunk(i: Int): OP_Foreach = {
    val restp = Targets.unitTypes(resultType)
    val r = new OP_Foreach(id+"_"+i, function, restp) //chunks all return Unit
    r.outputList = List(r.id)
    r.outputTypeMap = Map(r.id -> restp)
    r.inputList = inputList
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(r)
    for (c <- getConsumers) c.addDependency(r)
    r
  }

  def header(kernel: String, graph: DeliteTaskGraph): OP_Single = {
    val restp = Map(Targets.Scala->kernel)
    val h = new OP_Single(id+"_h", kernel, restp)
    //header assumes all inputs of map
    h.dependencyList = dependencyList
    h.outputList = List(h.id)
    h.outputTypeMap = Map(h.id -> restp)
    h.inputList = inputList
    h.addConsumer(this)
    for (dep <- getDependencies) dep.replaceConsumer(this, h)
    //map consumes header, map's consumers remain unchanged
    dependencyList = List(h)
    inputList = List((h,id+"_h"))

    graph.registerOp(h)
    //graph._ops += (id+"_h") -> h
    h
  }

  def nested = null
  def cost = 0
  def size = 0

}
