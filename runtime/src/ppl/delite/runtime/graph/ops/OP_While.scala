package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets

/**
 *
 */

class OP_While(val id: String,
               val predicateGraph: DeliteTaskGraph, val predicateValue: String,
               val bodyGraph: DeliteTaskGraph, val bodyValue: String)
  extends OP_Control {

  //currently support all targets
  def supportsTarget(target: Targets.Value) = true

  //does not produce output
  def outputType(target: Targets.Value) = target match {
    case Targets.Scala => "Unit"
    case Targets.Cuda => "void"
  }

  /**
   * creates a While chunk for each requested resource and destroys the original
   */
  def makeChunks(indices: Seq[Int]) = {
    val chunks =
      for (idx <- indices) yield {
        val r = new OP_While(id+"_"+idx, predicateGraph, predicateValue, bodyGraph, bodyValue)
        r.dependencyList = dependencyList //lists are immutable so can be shared
        r.inputList = inputList
        r.consumerList = consumerList
        for (dep <- getDependencies) dep.addConsumer(r)
        for (c <- getConsumers) c.addDependency(r)
        r
      }
    this.replaceAll(chunks(0))
    chunks
  }

}
