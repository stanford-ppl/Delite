package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/16/11
 * Time: 8:52 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_VariantScope extends OP_Control

class OP_BeginVariantScope(val id: String, resultType: Map[Targets.Value, String]) extends OP_VariantScope {
  val predicate = null
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_BeginVariantScope(id.dropRight(1)+"_"+idx+"b", Targets.unitTypes(resultType))
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }

  def variantOutputType(target: Targets.Value) = resultType(target)
  def variantOutputType = resultType(Targets.Scala)
}

//TODO: main chunk may need to be be scheduled on specific target & need to ensure internalResult is accessible from main chunk
class OP_EndVariantScope(val id: String, resultType: Map[Targets.Value, String], val result: DeliteOP) extends OP_VariantScope {
  val predicate = null
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_EndVariantScope(id+"_"+idx, Targets.unitTypes(resultType), result)
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }

  override def outputType(target: Targets.Value) = resultType(target)
}
