package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

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
    private[graph] var inputTypesMap = Map[Targets.Value,Map[String,String]]()
    def task = null
    def isDataParallel = false
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
