package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph
import scala.collection.mutable.{HashSet,HashMap}
/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:25 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Nested extends DeliteOP {

  def makeChunks(indices: Seq[Int], graph: DeliteTaskGraph): Seq[OP_Nested]

  def nestedGraphs: Seq[DeliteTaskGraph]
  
  final def task = functionName

  private var functionName = ""

  def setExecutableName(name: String) {
    functionName = name
  }

  protected final class GetterOp(val id: String, resource: Int, dependencies: Seq[DeliteOP], inputs: Seq[(DeliteOP,String)]) extends DeliteOP {

    for (dep <- dependencies) {
      this.addDependency(dep)
      dep.addConsumer(this)
    }
    for ((in,sym) <- inputs.reverse) {
      this.addInput(in, sym)
    }
    scheduledResource = resource

    private[graph] var outputTypesMap = Targets.unitTypes(id)
    def task = null
    def isDataParallel = false
  }

  // leave only those mutable inputs last mutated by current schedule
  def filterMutableInputs(idx: Int) {
    val mset = new HashSet[(DeliteOP,String)]
    val ops = nestedGraphs.flatMap(_.schedule.map(l => l.toArray).flatten)
    for(m <- mutableInputs) {
      val mutators = ops.filter(_.getMutableInputs.map(_._2).contains(m._2))
      val lastMutators = mutators.filter(m => mutators.filter(_.getDependencies.contains(m)).isEmpty)
      assert(lastMutators.length > 0)
      if(lastMutators.map(_.scheduledResource).distinct.length > 1) {
        lastMutators.find(_.scheduledResource == idx) match {
          case Some(mutator) => 
            mutator.mutableInputsCondition.get(m._2) match {
              case Some(lst) => mutableInputsCondition += Pair(m._2, lst) 
              case _ =>
            }
          case _ =>
        }
      }
      if(lastMutators.filter(_.scheduledResource == idx).nonEmpty) mset += m
    }
    mutableInputs = mset.toSet
    //val mset = nestedGraphs.flatMap(_.schedule(idx).toArray.flatMap(op=>op.getMutableInputs)).map(_._2).toSet
    //mutableInputs = mutableInputs filter (i => mset contains (i._2))    
  }

  def filterAntiDeps(idx: Int) {
    antiDeps = antiDeps.filter(d => d.getInputs.map(_._2).filter(mutableInputs.map(_._2).contains(_)).nonEmpty)
  }

  final def isDataParallel = false

  override def toString = id

  // Refining input dependencies
  // TODO: also refine other deps
  def refineInputDeps(nested: OP_Nested, graph:DeliteTaskGraph, idx: Int) {
    val internalOps = nested.nestedGraphs.flatMap(_.schedule(idx).toArray)
    var validInputs = for (in <- internalOps; (op,sym) <- in.getInputs; if (op.isInstanceOf[OP_Input])) yield (DeliteTaskGraph.getOp(sym)(graph), sym)
    validInputs ++= (for ((op,sym) <- nested.nestedGraphs.map(_.result); if (op.isInstanceOf[OP_Input] && op.scheduledResource==idx)) yield (DeliteTaskGraph.getOp(sym)(graph), sym))
    for (in <- nested.getInputs if !validInputs.contains(in)) {
      nested.removeInput(in._1, in._2)
    }
  }
}
