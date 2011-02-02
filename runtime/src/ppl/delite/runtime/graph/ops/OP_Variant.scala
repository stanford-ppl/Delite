package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:24 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Variant(val id: String, resultType: Map[Targets.Value,String], superOp: DeliteOP, val variantGraph: DeliteTaskGraph)
  extends OP_Nested {

  def supportsTarget(target: Targets.Value) = resultType.contains(target)

  def outputType(target: Targets.Value) = resultType(target)
  override def outputType: String = resultType(Targets.Scala)

  def returner(indices: Seq[Int]) = {
    if (variantGraph.result != null && !variantGraph.result.isInstanceOf[OP_Input])
      variantGraph.result.scheduledResource
    else indices(0)
  }

  def nestedGraphs = Seq(variantGraph)

  /**
   * creates a Variant chunk for each requested resource and destroys the original op
   */
  def makeChunks(indices: Seq[Int], graph: DeliteTaskGraph) = {
    var returnOp: OP_Variant = null
    val superOp = if (this.superOp == null) this else this.superOp
    val returnerIdx = returner(indices)
    val chunks =
      for (idx <- indices) yield {
        val resultMap = if (idx == returnerIdx) resultType else Targets.unitTypes
        val r = new OP_Variant(id+"_"+idx, resultMap, superOp, variantGraph)
        r.dependencyList = superOp.dependencyList
        r.inputList = superOp.inputList
        // TR FIXME: outputList
        r.consumerList = superOp.consumerList
        r.inputSyms = this.inputSyms
        r.cudaMetadata = this.cudaMetadata
        for (dep <- r.getDependencies) dep.addConsumer(r)
        for (c <- r.getConsumers) c.addDependency(r)
        if (idx == returnerIdx) returnOp = r

        //add special consumer ops
        if (resultMap(Targets.Scala) != "Unit") { //returns result and isReturner
          variantGraph.schedule(idx).add(new GetterOp(id+"v_"+idx, idx, Seq(variantGraph.result), Seq(variantGraph.result))) //get result on returner chunk
        }

        r
      }

    graph.replaceOp(superOp, returnOp)
    chunks
  }

}
