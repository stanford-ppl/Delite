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

  protected final class GetterOp(val id: String, resource: Int, dependencies: Seq[DeliteOP], inputs: Seq[DeliteOP]) extends DeliteOP {

    for (dep <- dependencies) {
      this.addDependency(dep)
      dep.addConsumer(this)
    }
    for (in <- inputs.reverse) {
      this.addInput(in, in.getOutputs.head)
    }
    scheduledResource = resource

    private[graph] val outputTypesMap = Targets.unitTypes(id)
    def task = null
    def isDataParallel = false
  }

  final def isDataParallel = false

}
