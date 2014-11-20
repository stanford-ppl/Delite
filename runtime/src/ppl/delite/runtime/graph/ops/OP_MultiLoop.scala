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

  def consumesHeader: Boolean = {
    inputList.length > 0 && (inputList(0)._2 == id+"_h")
  }

  // This strategy will try to distribute all multiloops with disjoint stencils, unless all of their array inputs are local
  // (locality-preserving, if possible). This could result in distributing tiny loops, which is will be very inefficient. We
  // may want to consider explicitly using size hints (or at the very least checking for a constant op size), and using that
  // to make the partitioning decision.
  override def partition = {
    val stencil = globalStencil()
    val inputPartitions = getInputs.map(i => i._1.outputPartition).toList
    val inputArrayPartitions = getInputArrayPartitions

    val desiredPartition =
      if (stencil == All /*|| stencil == Empty*/) Local // by partitioning "Empty" stencils are distributed, we are being very permissive
      else if (inputArrayPartitions.length > 0 && inputArrayPartitions.forall(_ == Local)) Local // if all our array inputs are local, we want to be too
      else Distributed(getOutputs)

    // Distributed if we chose distributed OR any of our inputs are distributed
    (desiredPartition :: inputPartitions).reduceLeft(_ combine _)
  }

  override def outputPartition = {
    if (needsCombine) Local // TODO: need to know needsCombine per output symbol rather than globally
    else partition
  }

  // We consider only inputArrayPartitions when optimizing for locality, instead of all inputs, because some Local inputs
  // are fine (can be broadcast, are field reads, etc.). An alternative to this strategy it to consider all inputs, but use
  // an explicit whitelist (see commented out method below).
  def getInputArrayPartitions = {
    // Ops in the task graph are mutable, and if we have been rewired to a header, we need to forward to the original inputs.
    val inputs = if (consumesHeader) getInputs.apply(0)._1.getInputs else getInputs
    val arrays = inputs.filter(i => i._1.outputIncludesDeliteArray)
    // Input delite arrays can come from struct unwrapping, which are local even when the array is distributed, so we have to check aliases
    arrays.flatMap(i => i._1.getAliases.map(_.outputPartition) + i._1.outputPartition)
  }

  // The wrappers are partitioned locally, but these can be run distributed. This whitelist serves a similar purpose
  // to the old OP_FileReader tag. We could also try to whitelist other operations here that we don't mind being local
  // (field accesses, struct creation).
  //
  // This is an alternative to getInputArrayPartitions: we only need it if we consider all inputs when deciding a partition.
  // def whitelist(inputs: Seq[DeliteOP]) = {
  //   inputs exists { o =>
  //     o.outputType == "generated.scala.io.DeliteFileInputStream"    ||
  //     o.outputType == "generated.scala.io.DeliteFileOutputStream"
  //   }
  // }

}
