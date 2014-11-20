/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.DeliteMesosScheduler
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph._
import scala.collection.immutable.SortedSet

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
    val r = new OP_MultiLoop(id+"_"+i, size, sizeIsConst, numDynamicChunks, numDynamicChunksIsConst, function, outputTypesMap, needsCombine, needsPostProcess)
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
    dependencies = SortedSet(h)
    inputList = List((h,h.id))
    graph.registerOp(h)
    h
  }

  // This strategy will try to distribute all multiloops with disjoint stencils, even though they might be small, which could be
  // very inefficient. Previously we only tried to distribute "input" ops (and ops that consumed them), which is one way of being
  // more specific. We may want to consider explicitly using size hints (or at the very least checking for a constant op size),
  // and using that to make the partitioning decision.
  //
  // The reasoning to move to default distributed was:
  //   1) We aren't currently using DeliteOpInput anymore
  //   2) It is better to distribute a small collection than try to not distribute a huge collection
  override def partition(sym: String) = {
    if (getInputs.isEmpty) Distributed(Set(id)) // default to Distributed when unconstrained
    else {
      val stencil = globalStencil()

      val desiredPartition =
        if (needsCombine) Local // TODO: need to know needsCombine per output symbol rather than globally
        else if (stencil == All /*|| stencil == Empty*/) Local // by partitioning "Empty" stencils are distributed, we are being very permissive
        else Distributed(getOutputs)

      val inputPartitions = getInputs.map(i => i._1.partition(i._2)).toList
      val chosenPartition = (desiredPartition :: inputPartitions).reduceLeft(_ combine _)

      chosenPartition
    }
  }

}
