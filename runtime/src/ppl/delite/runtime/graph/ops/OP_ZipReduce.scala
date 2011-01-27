package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

class OP_ZipReduce(val id: String, func: String, resultType: Map[Targets.Value,String]) extends OP_Executable(resultType) {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  /**
   * Since the semantics of ZipReduce are to return a T, all chunks are necessarily complete before the final T can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk(i: Int): OP_ZipReduce = {
    val r = new OP_ZipReduce(id+"_"+i, function, Targets.unitTypes(resultType))
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def header(kernel: String, graph: DeliteTaskGraph): OP_Single = {
    val h = new OP_Single(id+"_h", kernel, Map(Targets.Scala->kernel))
    //header assumes all inputs of zip
    h.dependencyList = dependencyList
    h.inputList = inputList
    h.addConsumer(this)
    for (dep <- getDependencies) dep.replaceConsumer(this, h)
    //zip consumes header, zip's consumers remain unchanged
    dependencyList = List(h)
    inputList = List(h)

    graph._ops += (id+"_h") -> h
    h
  }

  def cost = 0
  def size = 0

}
