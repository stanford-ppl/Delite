package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:25 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Nested extends DeliteOP {

  def makeChunks(indices: Seq[Int]): Seq[OP_Nested]

  final def task = functionName

  private var functionName = ""

  def setExecutableName(name: String) {
    functionName = name
  }

  protected final class GetterOp(val id: String, resource: Int, dependency: DeliteOP) extends DeliteOP {

    this.addDependency(dependency)
    dependency.addConsumer(this)
    scheduledResource = resource

    def supportsTarget(target: Targets.Value) = true
    def outputType(target: Targets.Value) = target match {
      case Targets.Scala => "Unit"
      case Targets.Cuda => "void"
    }

    def task = null
    def isDataParallel = false
    def cost = 0
    def size = 0
  }

  final def isDataParallel = false
  final def size = 0
  final def cost = 0

}
