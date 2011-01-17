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

  def outputSlotType(target: Targets.Value, name: String): String = {
//  if (outputTypeMap.isEmpty) return outputType(target)
    val m = outputTypeMap(name)
    m(target)
  }

  def outputSlotType(name: String): String = outputSlotType(Targets.Scala, name)

  def outputType(target: Targets.Value) : String
  def outputType : String = outputType(Targets.Scala)

  def hasCompoundOutput = getOutputs.nonEmpty && outputSlotType(getOutputs.head) != outputType
  // TODO improve check


  def supportsTarget(target: Targets.Value) : Boolean

  private[graph] var dependencyList: List[DeliteOP] = Nil //TR: should this be a set??

  final def getDependencies : Seq[DeliteOP] = dependencyList

  final def addDependency(dep: DeliteOP) {
    dependencyList = dep :: dependencyList
  }

  private[graph] var consumerList: List[DeliteOP] = Nil

  final def getConsumers : Seq[DeliteOP] = consumerList

  final def addConsumer(c: DeliteOP) {
    consumerList = c :: consumerList
  }

  final def replaceConsumer(old: DeliteOP, c: DeliteOP) {
    consumerList = c :: (consumerList filterNot { _ == old })
  }


  private[graph] var outputList: List[String] = Nil
  private[graph] var outputTypeMap: Map[String,Map[Targets.Value, String]] = Map.empty

  final def getOutputs : Seq[String] = outputList

  final def addOutput(output: String, tp: Map[Targets.Value, String]) {
    outputList = output :: outputList
    outputTypeMap += (output -> tp)
  }



  //this is a subset of getDependencies and contains the inputs in the order required to call the task
  private[graph] var inputList: List[(DeliteOP, String)] = Nil

  final def getInputs : Seq[(DeliteOP, String)] = inputList

  final def addInput(op: DeliteOP): Unit = addInput(op, op.outputList(0))
  
  final def addInput(op: DeliteOP, name: String) {
    assert(op.outputList.contains(name), "Op " + op + " does not have output " + name)
    inputList = (op, name) :: inputList
  }

  def id: String

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

}