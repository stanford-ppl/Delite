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

  def outputType(target: Targets.Value) : String
  def outputType : String = outputType(Targets.Scala)

  def supportsTarget(target: Targets.Value) : Boolean

  //list of all incoming graph edges for this op
  private[graph] var dependencyList: List[DeliteOP] = Nil

  final def getDependencies : Seq[DeliteOP] = dependencyList

  final def addDependency(dep: DeliteOP) {
    dependencyList = dep :: dependencyList
  }

  final def removeDependency(dep: DeliteOP) {
    dependencyList = dependencyList filterNot { _ == dep }
  }

  final def replaceDependency(old: DeliteOP, dep: DeliteOP) {
    dependencyList = dep :: (dependencyList filterNot { _ == old })
  }

  //list of all outgoing graph edges for this op
  private[graph] var consumerList: List[DeliteOP] = Nil

  final def getConsumers : Seq[DeliteOP] = consumerList

  final def addConsumer(c: DeliteOP) {
    consumerList = c :: consumerList
  }

  final def removeConsumer(c: DeliteOP) {
    consumerList = consumerList filterNot { _ == c }
  }

  final def replaceConsumer(old: DeliteOP, c: DeliteOP) {
    consumerList = c :: (consumerList filterNot { _ == old })
  }

  //this is a subset of dependencies and contains the kernel inputs in the order required to call the task
  private[graph] var inputList: List[DeliteOP] = Nil

  final def getInputs : Seq[DeliteOP] = inputList

  final def addInput(input: DeliteOP) {
    inputList = input :: inputList
  }

  final def replaceInput(old: DeliteOP, input: DeliteOP) {
    inputList = inputList.patch(inputList.indexOf(old), List(input), 1)
    if (mutableInputList contains old) mutableInputList = input :: (mutableInputList filterNot { _ == old })
  }

  //this is a subset of inputs and contains only the inputs that the op can mutate
  private[graph] var mutableInputList: List[DeliteOP] = Nil

  final def getMutableInputs : Seq[DeliteOP] = mutableInputList

  final def addMutableInput(input: DeliteOP) {
    mutableInputList = input :: mutableInputList
  }

  var variant: DeliteTaskGraph = null

  def id: String

  def nested : DeliteTaskGraph

  def cost: Int

  def size: Int

  //TODO: more versatile/useful to match on the specific type of OP rather than simply dataParallel/sequential buckets?
  //TODO: should revisit this when we have more complex dataParallel patterns
  def isDataParallel : Boolean

  //TODO: do all OP subtypes support CUDA? (maybe shouldn't be here)
  var cudaMetadata = new CudaMetadata

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

}
