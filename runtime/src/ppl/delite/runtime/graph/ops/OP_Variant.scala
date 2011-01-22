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

class OP_Variant(val id: String, resultType: Map[Targets.Value,String], superOp: DeliteOP, val variantGraph: DeliteTaskGraph) extends OP_Nested {

  def supportsTarget(target: Targets.Value) = resultType.contains(target)

  def outputType(target: Targets.Value) = resultType(target)
  override def outputType: String = resultType(Targets.Scala)

  def isReturner(idx: Int) = {
    (variantGraph.result.scheduledResource == idx)
  }

  /**
   * creates a Variant chunk for each requested resource and destroys the original op
   */
  def makeChunks(indices: Seq[Int]) = {
    var returner: OP_Variant = null
    val chunks =
      for (idx <- indices) yield {
        val resultMap = if (isReturner(idx)) resultType else Targets.unitTypes
        val r = new OP_Variant(id+"_"+idx, resultMap, superOp, variantGraph)
        r.dependencyList = superOp.dependencyList
        r.inputList = this.inputList ::: superOp.inputList
        r.consumerList = superOp.consumerList
        r.inputSyms = this.inputSyms
        for (dep <- r.getDependencies) dep.addConsumer(r)
        for (c <- r.getConsumers) c.addDependency(r)
        if (isReturner(idx)) returner = r

        //add special consumer ops
        if (resultMap(Targets.Scala) != "Unit") { //returns result and isReturner
          variantGraph.schedule(idx).add(new GetterOp(id+"v_"+idx, idx, variantGraph.result)) //get result on returner chunk
        }

        r
      }

    superOp.replaceAll(returner)
    chunks
  }

}
