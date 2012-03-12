package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets

/**
 *
 */

class OP_Condition(val id: String, private[graph] val outputTypesMap: Map[Targets.Value, Map[String,String]],
                   val predicateGraph: DeliteTaskGraph, val predicateValue: String,
                   val thenGraph: DeliteTaskGraph, val thenValue: String,
                   val elseGraph: DeliteTaskGraph, val elseValue: String, val isReturner: Boolean)
  extends OP_Control {

  def nestedGraphs = Seq(predicateGraph, thenGraph, elseGraph)

  def returner(indices: Seq[Int]) = {
    /*
    if (thenGraph.result._1 != null && !thenGraph.result._1.isInstanceOf[OP_Input])
      thenGraph.result._1.scheduledResource
    else if (elseGraph.result._1 != null && !elseGraph.result._1.isInstanceOf[OP_Input])
      elseGraph.result._1.scheduledResource
    else indices(0)
    */
    // Returner is always 0 (assuming the index 0 is CPU)
    //TODO: Change this so that GPU can also be the returner
    indices(0)
  }

  /**
   * creates a Condition chunk for each requested resource and destroys the original
   */
  def makeChunks(indices: Seq[Int], graph: DeliteTaskGraph) = {
    var returnOp: OP_Condition = null
    val returnerIdx = returner(indices)
    val chunks =
      for (idx <- indices) yield {
        val resultMap = if (idx == returnerIdx) outputTypesMap else Targets.unitTypes(id+"_"+idx)
        val r = new OP_Condition(id+"_"+idx, resultMap, predicateGraph, predicateValue,
        thenGraph, thenValue, elseGraph, elseValue, idx == returnerIdx)
        r.dependencies = dependencies
        r.inputList = inputList
        r.mutableInputs = mutableInputs
        r.consumers = consumers
        for (tgt <- Targets.GPU) r.setGPUMetadata(tgt, getGPUMetadata(tgt))
        for (dep <- getDependencies) dep.addConsumer(r)
        for (c <- getConsumers) c.addDependency(r)
        if (idx == returnerIdx) returnOp = r

        //add special consumer ops
        if (predicateValue == "") predicateGraph.schedule(idx).add(new GetterOp(id+"p_"+idx, idx, Seq(predicateGraph.result._1), Seq(predicateGraph.result._1))) //get predicate result on all chunks
        if (r.outputType != "Unit") { //returns result and isReturner
          if (thenValue == "") thenGraph.schedule(idx).add(new GetterOp(id+"t_"+idx, idx, Seq(thenGraph.result._1), Seq(thenGraph.result._1))) //get then result on returner chunk
          if (elseValue == "") elseGraph.schedule(idx).add(new GetterOp(id+"e_"+idx, idx, Seq(elseGraph.result._1), Seq(elseGraph.result._1))) //get else result on returner chunk
        }
        r
      }

    graph.replaceOp(this, returnOp)
    chunks
  }

}
