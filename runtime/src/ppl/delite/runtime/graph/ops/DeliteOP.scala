package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets._

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:33:29 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteOP {

  /**
   * these methods should be instantiated from parsing the Delite Execution Graph input
   */
  def task : String

  private[graph] val outputTypesMap: Map[Targets.Value, Map[String,String]]

  def outputType(target: Targets.Value, symbol: String): String = outputTypesMap(target)(symbol)
  def outputType(target: Targets.Value): String = outputTypesMap(target)("functionReturn")
  def outputType(symbol: String) = outputTypesMap(Targets.Scala)(symbol)
  def outputType = outputTypesMap(Targets.Scala)("functionReturn")

  def supportsTarget(target: Targets.Value) : Boolean = outputTypesMap contains target

  def getOutputs = outputTypesMap.head._2.keySet - "functionReturn"

  //set of all incoming graph edges for this op
  private[graph] var dependencies = Set.empty[DeliteOP]

  final def getDependencies : Set[DeliteOP] = dependencies

  final def addDependency(dep: DeliteOP) {
    dependencies += dep
  }

  final def removeDependency(dep: DeliteOP) {
    dependencies -= dep
  }

  final def replaceDependency(old: DeliteOP, dep: DeliteOP) {
    assert(dependencies contains old, old.toString + " is not a dependency of " + this.toString + "; cannot be replaced")
    dependencies -= old
    dependencies += dep
  }

  //set of all outgoing graph edges for this op
  private[graph] var consumers = Set.empty[DeliteOP]

  final def getConsumers : Set[DeliteOP] = consumers

  final def addConsumer(c: DeliteOP) {
    consumers += c
  }

  final def removeConsumer(c: DeliteOP) {
    consumers -= c
  }

  final def replaceConsumer(old: DeliteOP, c: DeliteOP) {
    assert(consumers contains old, old.toString + " is not a consumer of " + this.toString + ", cannot be replaced")
    consumers -= old
    consumers += c
  }

  //this is a subset of dependencies and contains the kernel inputs in the order required to call the task
  private[graph] var inputList: List[(DeliteOP, String)] = Nil

  final def getInputs : Seq[(DeliteOP, String)] = inputList

  final def addInput(op: DeliteOP, name: String) {
    inputList = (op, name) :: inputList
  }

  final def replaceInput(old: DeliteOP, input: DeliteOP, name: String) {
    inputList.find(_ == (old, name)) match {
      case Some(oldPair) => {
        assert(input.outputTypesMap.head._2.contains(name), "Cannot replace " + old + " with " + input + " as it does not contain output " + name)
        inputList = inputList.patch(inputList.indexOf(oldPair), List((input, name)), 1)
      }
      case None => error(old + " is not an input of " + this + "; cannot be replaced")
    }
    if (mutableInputs.contains((old, name))) {
      mutableInputs -= Pair(old, name)
      mutableInputs += Pair(input, name)
    }
  }

  //subset of inputs containing only the inputs that the op can mutate
  private[graph] var mutableInputs = Set.empty[(DeliteOP, String)]

  final def getMutableInputs : Set[(DeliteOP, String)] = mutableInputs

  final def addMutableInput(op: DeliteOP, name: String) {
    mutableInputs += Pair(op, name)
  }

  var variant: OP_Variant = null

  def id: String

  //TODO: more versatile/useful to match on the specific type of OP rather than simply dataParallel/sequential buckets?
  //TODO: should revisit this when we have more complex dataParallel patterns
  def isDataParallel : Boolean

  private var cudaMetadata: GPUMetadata = new CudaMetadata
  private var openclMetadata: GPUMetadata = new OpenCLMetadata
  def getGPUMetadata(tgt: Targets.Value): GPUMetadata = tgt match {
    case Targets.Cuda => cudaMetadata
    case Targets.OpenCL => openclMetadata
    case _ => throw new IllegalArgumentException("unsupported target for metadata: " + tgt)
  }
  def setGPUMetadata(tgt: Targets.Value, metadata: GPUMetadata) {
    tgt match {
      case Targets.Cuda => cudaMetadata = metadata
      case Targets.OpenCL => openclMetadata = metadata
      case _ => throw new IllegalArgumentException("unsupported target for metadata: " + tgt)
    }
  }


  /**
   * these methods/state are used for scheduling
   */
  var isSchedulable = false

  var isScheduled = false

  def processSchedulable {
    var free = true
    for (dep <- getDependencies) {
      free &&= dep.isScheduled
    }
    if (free) isSchedulable = true
  }

  /**
   * these methods/state are used for code generation
   */
  var scheduledResource = -1

  override def toString = id

}
