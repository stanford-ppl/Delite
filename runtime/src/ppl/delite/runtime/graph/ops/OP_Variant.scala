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

class OP_Variant(val id: String, resultType: Map[Targets.Value,String], val variantGraph: DeliteTaskGraph) extends OP_Nested {

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
        val r = new OP_Variant(id+"_"+idx, resultMap, variantGraph)
        r.dependencyList = dependencyList
        r.inputList = inputList
        r.consumerList = consumerList
        for (dep <- getDependencies) dep.addConsumer(r)
        for (c <- getConsumers) c.addDependency(r)
        if (isReturner(idx)) returner = r

        //add special consumer ops
        if (resultMap(Targets.Scala) != "Unit") { //returns result and isReturner
          variantGraph.schedule(idx).add(new GetterOp(id+"v_"+idx))
        }

        r
      }

    this.replaceAll(returner)
    chunks
  }

  private class GetterOp(val id: String) extends DeliteOP {

    def supportsTarget(target: Targets.Value) = true
    def outputType(target: Targets.Value) = target match {
      case Targets.Scala => "Unit"
      case Targets.Cuda => "void"
    }

    def task = ""
    def isDataParallel = false
    def cost = 0
    def size = 0
  }

}
