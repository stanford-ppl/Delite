package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets
import scala.collection.mutable.HashSet

/**
 *
 */

class OP_Condition(val id: String, private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]],
                   val predicateGraph: DeliteTaskGraph, val predicateValue: String,
                   val thenGraph: DeliteTaskGraph, val thenValue: String,
                   val elseGraph: DeliteTaskGraph, val elseValue: String)
  extends OP_Control {

  def nestedGraphs = Seq(predicateGraph, thenGraph, elseGraph)

  def returner(indices: Seq[Int]) = {

    if (thenGraph.result._1 != null && !thenGraph.result._1.isInstanceOf[OP_Input])
      thenGraph.result._1.scheduledResource
    else if (elseGraph.result._1 != null && !elseGraph.result._1.isInstanceOf[OP_Input])
      elseGraph.result._1.scheduledResource
    else indices(0)

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
        thenGraph, thenValue, elseGraph, elseValue)
        r.dependencies = dependencies
        r.inputList = inputList
        r.mutableInputs = mutableInputs
        r.filterMutableInputs(idx)
        r.antiDeps = antiDeps
        r.filterAntiDeps(idx)
        r.consumers = consumers

        for (tgt <- Targets.GPU) r.setGPUMetadata(tgt, getGPUMetadata(tgt))
        for (dep <- getDependencies) dep.addConsumer(r)
        for (c <- getConsumers) c.addDependency(r)
        if (idx == returnerIdx) returnOp = r

        //add special consumer ops
        if (predicateValue == "") predicateGraph.schedule(idx).add(new GetterOp(id+"p_"+idx, idx, Seq(predicateGraph.result._1), Seq(predicateGraph.result))) //get predicate result on all chunks
        if (r.outputType != "Unit") { //returns result and isReturner
          if (thenValue == "") thenGraph.schedule(idx).add(new GetterOp(id+"t_"+idx, idx, Seq(thenGraph.result._1), Seq(thenGraph.result))) //get then result on returner chunk
          if (elseValue == "") elseGraph.schedule(idx).add(new GetterOp(id+"e_"+idx, idx, Seq(elseGraph.result._1), Seq(elseGraph.result))) //get else result on returner chunk
        }
        r
      }

    graph.replaceOp(this, returnOp)
    for (i <- 0 until indices.length)
      refineInputDeps(chunks(i), graph, indices(i))
    chunks
  }

  // leave only those mutable inputs last mutated by current schedule
  override def filterMutableInputs(idx: Int) {
    val mset = new HashSet[(DeliteOP,String)]
    val ops = nestedGraphs.flatMap(_.schedule.map(l => l.toArray).flatten)
    for(m <- mutableInputs) {
      val thenOps = thenGraph.schedule.map(_.toArray).flatten
      val elseOps = elseGraph.schedule.map(_.toArray).flatten
      val thenMutators = thenOps.filter(_.getMutableInputs.map(_._2).contains(m._2))
      val elseMutators = elseOps.filter(_.getMutableInputs.map(_._2).contains(m._2))
      // should be either one mutator or none
      val thenLastMutator = thenMutators.find(m => thenMutators.filter(_.getDependencies.contains(m)).isEmpty)
      val elseLastMutator = elseMutators.find(m => elseMutators.filter(_.getDependencies.contains(m)).isEmpty)
       
      (thenLastMutator, elseLastMutator) match {
        case (Some(tm),Some(em)) if(tm.scheduledResource==idx || em.scheduledResource==idx) =>
          mset += m
          if(tm.scheduledResource!=em.scheduledResource) {
            val cond = tm.scheduledResource == idx
            val mutator = if(cond) tm else em
            mutator.mutableInputsCondition.get(m._2) match {
              case Some(lst) => mutableInputsCondition += Pair(m._2, lst:+(this,cond))
              case _ => mutableInputsCondition += Pair(m._2, List((this,cond)))
            }
          }
        case (Some(tm),None) if(tm.scheduledResource==idx) => mset += m 
        case (None,Some(em)) if(em.scheduledResource==idx) => mset += m
        case _ =>  
      }
    }
    mutableInputs = mset.toSet
  }

}
