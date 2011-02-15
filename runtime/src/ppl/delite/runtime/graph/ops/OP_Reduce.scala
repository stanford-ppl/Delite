package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:27:00 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Reduce(val id: String, func: String, resultType: Map[Targets.Value,String]) extends OP_Executable(resultType) {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  /**
   * Since the semantics of Reduce are to return a T, all chunks are necessarily complete before the final T can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk(i: Int): OP_Reduce = {
    val restp = Targets.unitTypes(resultType)
    val r = new OP_Reduce(id+"_"+i, function, restp) //chunks all return Unit
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.outputList = List(r.id)
    r.outputTypeMap = Map(r.id -> restp)
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
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
    inputList = List((h,h.id))

    graph.registerOp(h)
    //graph._ops += (id+"_h") -> h
    h
  }

  def cost = 0
  def size = 0

}