/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph._

class OP_MultiLoop(val id: String, val size: String, val sizeIsConst: Boolean, val numDynamicChunks: String, val numDynamicChunksIsConst: Boolean, func: String, private[graph] var outputTypesMap: Map[Targets.Value,Map[String,String]], val needsCombine: Boolean, val needsPostProcess: Boolean) extends OP_Executable {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  /**
   * Since the semantics of the multiloop are to mutate the elements in a collection all consumer (true) dependency edges already exist in graph
   * It is the responsibility of the multiloop to ensure all chunks are complete before the master chunk returns the result (chunks have no external consumers)
   * Chunks require same dependency & input lists (the header)
   */
  def chunk(i: Int): OP_MultiLoop = {
	  // TODO: aks (pass a sane size, sizeIsConst for the chunk?)
    val r = new OP_MultiLoop(id+"_"+i, size, sizeIsConst, numDynamicChunks, numDynamicChunksIsConst, function, Targets.unitTypes(id+"_"+i, outputTypesMap), needsCombine, needsPostProcess) //chunks all return Unit
    r.dependencies = dependencies //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def header(kernel: String, outputType: String, graph: DeliteTaskGraph): OP_Single = {
    val typesMap = Map(id+"_h"->outputType,"functionReturn"->outputType)
    val headerTypesMap = Map(Targets.Scala->typesMap, Targets.Cpp->typesMap)
    val h = new OP_Single(id+"_h", kernel, headerTypesMap)
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

  override def partition = { //TODO: "free" partition could be local or distributed...
    if (getInputs.isEmpty) Local
    else getInputs.map(i => i._1.partition(i._2)).reduceLeft(_ combine _)
  }

  override def partition(sym: String) = {
    val opPartition = partition
    if (needsCombine) Local //TODO: need to know needsCombine per output symbol rather than globally
    else if (needsPostProcess) { //TODO: likewise for needsPostProcess
      if (opPartition == Local) Local
      else Distributed(Set(id)) //fresh logical partition 
    }
    else opPartition
  }

}
