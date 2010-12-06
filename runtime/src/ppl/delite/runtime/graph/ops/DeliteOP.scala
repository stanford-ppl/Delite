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

  protected var dependencyList: List[DeliteOP] = Nil

  final def getDependencies : Seq[DeliteOP] = dependencyList

  final def addDependency(dep: DeliteOP) {
    dependencyList = dep :: dependencyList
  }

  protected var consumerList: List[DeliteOP] = Nil

  final def getConsumers : Seq[DeliteOP] = consumerList

  final def addConsumer(c: DeliteOP) {
    consumerList = c :: consumerList
  }

  //this is a subset of getDependencies and contains the inputs in the order required to call the task
  protected var inputList: List[DeliteOP] = Nil

  final def getInputs : Seq[DeliteOP] = inputList

  final def addInput(input: DeliteOP) {
    inputList = input :: inputList
  }

  def nested : DeliteTaskGraph

  def cost: Int

  def size: Int

  //TODO: more versatile/useful to match on the specific type of OP rather than simply dataParallel/sequential buckets?
  //TODO: should revisit this when we have more complex dataParallel patterns
  def isDataParallel : Boolean

  //TODO: do all OP subtypes support CUDA? (maybe shouldn't be here)
  val cudaMetadata = new CudaMetadata

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

  //TODO: need an implementation that *guarantees* a unique ID for every OP object in the system
  //TODO: could be assigned while parsing DEG input (e.g., related to kernel ID)
  def id: Int = {
    System.identityHashCode(this)
  }

}