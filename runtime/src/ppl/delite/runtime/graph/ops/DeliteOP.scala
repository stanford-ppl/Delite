package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph._
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

  private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]]
  private[graph] val stencilMap = new collection.mutable.HashMap[String,Stencil]()
  private[graph] val supportedTargets = new collection.mutable.HashSet[Targets.Value]

  def getOutputTypesMap = outputTypesMap
  def outputType(target: Targets.Value, symbol: String): String = outputTypesMap(target)(symbol)
  def outputType(target: Targets.Value): String = outputTypesMap(target)("functionReturn")
  def outputType(symbol: String) = outputTypesMap(Targets.Scala)(symbol)
  def outputType = outputTypesMap(Targets.Scala)("functionReturn")
  def inputType(target: Targets.Value, symbol: String): String = (getInputs ++ getMutableInputs).find(_._2 == symbol).get._1.outputType(target,symbol)
  def inputType(symbol: String): String = inputType(Targets.Scala, symbol)

  def supportsTarget(target: Targets.Value): Boolean = supportedTargets contains target

  def getOutputs = outputTypesMap.head._2.keySet - "functionReturn"

  def stencil(symbol: String) = stencilMap(symbol)
  def stencilOrElse(symbol: String)(orElse: => Stencil) = stencilMap.getOrElse(symbol, orElse)

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
    if (antiDeps.contains(c)) {
      antiDeps -= c
    }
  }

  final def replaceConsumer(old: DeliteOP, c: DeliteOP) {
    assert(consumers contains old, old.toString + " is not a consumer of " + this.toString + ", cannot be replaced")
    consumers -= old
    consumers += c
    if (antiDeps.contains(c)) {
      antiDeps -= old
      antiDeps += c
    }
  }

  //this is a subset of dependencies and contains the kernel inputs in the order required to call the task
  private[graph] var inputList: List[(DeliteOP, String)] = Nil

  final def getInputs : Seq[(DeliteOP, String)] = inputList

  final def addInput(op: DeliteOP, name: String) {
    inputList = (op, name) :: inputList
  }

  final def removeInput(op: DeliteOP, name: String) {
    inputList = inputList.filterNot(_ == (op,name))
    if (mutableInputs.contains((op, name))) {
      mutableInputs -= Pair(op, name)
    }
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
  
  //mapping from mutated symbol to the condition list of nested blocks that make the mutation happen
  val mutableInputsCondition = new collection.mutable.HashMap[String, List[(DeliteOP,Boolean)]]

  //subset of dependencies for anti-deps
  private[graph] var antiDeps = Set.empty[DeliteOP]

  final def getAntiDeps: Set[DeliteOP] = antiDeps
  final def addAntiDep(op: DeliteOP) {
    antiDeps += op
  } 

  def id: String

  //TODO: more versatile/useful to match on the specific type of OP rather than simply dataParallel/sequential buckets?
  //TODO: should revisit this when we have more complex dataParallel patterns
  def isDataParallel : Boolean

  def partition: Partition = Local
  def partition(symbol: String): Partition = partition

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
  def scheduledOn(target: Targets.Value): Boolean = {
    if (Targets.resourceIDs(target) contains scheduledResource) true
    else false
  }
}
