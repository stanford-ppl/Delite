package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.{CudaMetadata, Targets}

abstract class DeliteOP extends DeliteOPBase {

  /**
   * these methods should be instantiated from parsing the Delite Execution Graph input
   */
  def task : String

  def outputType(target: Targets.Value) : String
  def outputType : String = outputType(Targets.Scala)

  def supportsTarget(target: Targets.Value) : Boolean

  //this is a subset of getDependencies and contains the inputs in the order required to call the task
  private[graph] var inputList: List[DeliteOP] = Nil

  final def getInputs : Seq[DeliteOP] = inputList

  final def addInput(input: DeliteOP) {
    inputList = input :: inputList
  }

  //TODO: more versatile/useful to match on the specific type of OP rather than simply dataParallel/sequential buckets?
  //TODO: should revisit this when we have more complex dataParallel patterns
  def isDataParallel : Boolean

  //TODO: do all OP subtypes support CUDA? (maybe shouldn't be here)
  val cudaMetadata = new CudaMetadata


}