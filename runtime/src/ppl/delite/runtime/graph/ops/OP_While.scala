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

  def nestedGraphs = Seq(predicateGraph, bodyGraph)

  /**
   * creates a While chunk for each requested resource and destroys the original
   */
  def makeChunks(indices: Seq[Int], graph: DeliteTaskGraph) = {
    val lastOps = if (bodyValue == "") for (r <- bodyGraph.schedule if(!r.isEmpty)) yield r.peekLast else null
    val chunks =
      for (idx <- indices) yield {
        val r = new OP_While(id+"_"+idx, predicateGraph, predicateValue, bodyGraph, bodyValue)
        r.dependencyList = dependencyList
        r.inputList = inputList
        r.consumerList = consumerList
        r.inputSyms = inputSyms
        r.cudaMetadata = cudaMetadata
        for (dep <- getDependencies) dep.addConsumer(r)
        for (c <- getConsumers) c.addDependency(r)

        //add special consumer ops
        if (predicateValue == "") predicateGraph.schedule(idx).add(new GetterOp(id+"p_"+idx, idx, Seq(predicateGraph.result), Seq(predicateGraph.result))) //get predicate result on all chunks
        if (bodyValue == "") bodyGraph.schedule(idx).add(new GetterOp(id+"b_"+idx, idx, lastOps, Seq())) //barrier end of body so predicate can be reevaluated

        r
      }

    graph.replaceOp(this, chunks(0))
    chunks
  }

}
