/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

class OP_MultiLoop(val id: String, val size: String, val sizeIsConst: Boolean, func: String, private[graph] val outputTypesMap: Map[Targets.Value,Map[String,String]], val needsCombine: Boolean, val needsPostProcess: Boolean) extends OP_Executable {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  /**
   * Since the semantics of the multiloop are to mutate the elements in a collection all consumer (true) dependency edges already exist in graph
   * Chunking needs to add additional anti-dependency edges for each chunk to ensure all chunks are complete,
   *  unless a combine is performed, in which case all chunks are necessarily complete when final result is returned
   * Chunks require same dependency & input lists
   */
  def chunk(i: Int): OP_MultiLoop = {
	  // TODO: aks (pass a sane size, sizeIsConst for the chunk?)
    val r = new OP_MultiLoop(id+"_"+i, size, sizeIsConst, function, Targets.unitTypes(id+"_"+i, outputTypesMap), needsCombine, needsPostProcess) //chunks all return Unit
    r.dependencies = dependencies //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    if (!needsCombine) {
      r.consumers = consumers
      for (c <- getConsumers) c.addDependency(r)
    }
    r
  }

  def header(kernel: String, graph: DeliteTaskGraph): OP_Single = {
    val h = new OP_Single(id+"_h", kernel, Map(Targets.Scala->Map(id+"_h"->kernel,"functionReturn"->kernel)))
    //header assumes all inputs of multiloop
    h.dependencies = dependencies
    h.inputList = inputList
    h.addConsumer(this)
    for (dep <- getDependencies) dep.replaceConsumer(this, h)
    //map consumes header, multiloop's consumers remain unchanged
    dependencies = Set(h)
    inputList = List((h,h.id))
    graph.registerOp(h)
    h
  }

}
