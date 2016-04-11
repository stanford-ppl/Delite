package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph._
import ppl.delite.runtime.graph.targets._
import scala.collection.immutable.SortedSet

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:33:29 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object DeliteOP {
  //the ordering is arbitary, we just want the generated code to be consistent for the sake of the code cache
  implicit val deliteOpOrdering: Ordering[DeliteOP] = Ordering.by(_.id)
}

abstract class DeliteOP {

  /**
   * these methods should be instantiated from parsing the Delite Execution Graph input
   */
  def task : String

  // Name of the op, along with getter and setter methods
  private var __opName: String = ""
  def opName: String = __opName
  def opName_=(name: String) {
    __opName = name
  }

  private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]]
  private[graph] val stencilMap = new collection.mutable.HashMap[String,Stencil]()
  private[graph] val supportedTargets = new collection.mutable.HashSet[Targets.Value]

  def getOutputTypesMap = outputTypesMap
  def outputType(target: Targets.Value, symbol: String): String = {
    val tmap = outputTypesMap.getOrElse(target, sys.error("op " + this.toString + " does not have outputs defined for target " + target))
    tmap.getOrElse(symbol, sys.error("op " + this.toString + " does not have an output defined for symbol " + symbol))
  }
  def outputType(target: Targets.Value): String = outputType(target, "functionReturn")
  def outputType(symbol: String): String = outputType(Targets.Scala, symbol)
  def outputType = outputTypesMap(Targets.Scala)("functionReturn")
  def inputType(target: Targets.Value, symbol: String): String = (getInputs ++ getMutableInputs).find(_._2 == symbol).get._1.outputType(target,symbol)
  def inputType(symbol: String): String = inputType(Targets.Scala, symbol)

  def supportsTarget(target: Targets.Value): Boolean = supportedTargets contains target

  def getOutputs = SortedSet.empty[String] ++ (outputTypesMap.head._2.keySet - "functionReturn")
  def outputIncludesDeliteArray = getOutputTypesMap(Targets.Scala).values.exists(t => t.contains("runtime.data.DeliteArray"))

  def stencil(symbol: String) = stencilMap(symbol)
  def stencilOrElse(symbol: String)(orElse: => Stencil) = stencilMap.getOrElse(symbol, orElse)

  // set of all aliases for this op
  private[graph] var aliases = SortedSet.empty[DeliteOP]

  final def getAliases : Set[DeliteOP] = aliases

  final def addAlias(a: DeliteOP) {
    aliases += a
  }

  //set of all incoming graph edges for this op
  private[graph] var dependencies = SortedSet.empty[DeliteOP]

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
  private[graph] var consumers = SortedSet.empty[DeliteOP]

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
    if (antiDeps.contains(old)) {
      antiDeps -= old
      antiDeps += c
    }
  }

  //this is a subset of dependencies and contains the kernel inputs in the order required to call the task
  private[graph] var inputList: List[(DeliteOP, String)] = Nil

  final def getInputs : Seq[(DeliteOP, String)] = inputList
  final def getInputSet: Set[(DeliteOP, String)] = SortedSet(inputList:_*)

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
  private[graph] var mutableInputs = SortedSet.empty[(DeliteOP, String)]

  final def getMutableInputs : Set[(DeliteOP, String)] = mutableInputs

  final def addMutableInput(op: DeliteOP, name: String) {
    mutableInputs += Pair(op, name)
  }

  //mapping from mutated symbol to the condition list of nested blocks that make the mutation happen
  val mutableInputsCondition = new collection.mutable.HashMap[String, List[(DeliteOP,Boolean)]]

  //subset of dependencies for anti-deps
  private[graph] var antiDeps = SortedSet.empty[DeliteOP]

  final def getAntiDeps: Set[DeliteOP] = antiDeps
  final def addAntiDep(op: DeliteOP) {
    antiDeps += op
  }

  def id: String

  //TODO: more versatile/useful to match on the specific type of OP rather than simply dataParallel/sequential buckets?
  //TODO: should revisit this when we have more complex dataParallel patterns
  def isDataParallel : Boolean


  /**
   * Partitioning
   */

  // We try to be a little more precise here w.r.t. aliases that we consider in the stencil.
  // The idea is we only care about aliases that are DeliteArrays or possibly those with type "Unit",
  // as these can be transitive alias connectors (e.g. var assignment).
  def getArrayAliases: Set[DeliteOP] = {
    getAliases filter { a => a.outputIncludesDeliteArray || a.outputType == "Unit" }
  }

  // lookup all ops that consume this op and compute a join over their stencils
  def globalStencil(excludeAliases: Set[DeliteOP] = Set.empty): Stencil = {
    // we consider each output individually, since if we just use "this.id",
    // that can return a fused id, which we do not have stencils for
    val outputs = getOutputs

    // collect relevant stencils from all ops that consume 1 or more of our outputs
    def outputStencilsFrom(o: DeliteOP): Set[Stencil] = o match {
      case nested:OP_Nested =>
        nested.nestedGraphs.toSet.flatMap((g: DeliteTaskGraph) => g.ops.flatMap(o => outputStencilsFrom(o)))
      case _ =>
        val consumes = outputs intersect o.getInputs.map(_._2).toSet
        consumes.map(i => o.stencilOrElse(i)(Empty))
    }

    val consumerStencils = getConsumers flatMap { op => outputStencilsFrom(op) }
    val aliasStencils = (getArrayAliases diff excludeAliases) map { op => op.globalStencil(excludeAliases + this) }
    (consumerStencils ++ aliasStencils).foldLeft(Empty: Stencil)(_ combine _ )
  }

  // Where to schedule this op
  def partition: Partition = Local

  // Where this ops outputs will live after its finished (e.g. reduces becomes local)
  def outputPartition: Partition = Local


  /**
   * GPU
   */
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
