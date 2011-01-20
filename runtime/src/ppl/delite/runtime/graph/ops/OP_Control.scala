package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/5/11
 * Time: 6:58 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Control extends DeliteOP {

  val predicate: DeliteOP

  def makeChunk(idx: Int): OP_Control

  //control structures currently support all targets
  def supportsTarget(target: Targets.Value) = true

  //control structures do not produce output
  def outputType(target: Targets.Value) = target match {
    case Targets.Scala => "Unit"
    case Targets.Cuda => "void"
  }

  // TODO: replace these by correct tracking of outputs
  override def getOutputs = List(id)
  override def outputSlotType(target: Targets.Value, name: String) = outputType(target)
  override def addOutput(output: String, tp: Map[Targets.Value, String]) = system.error("not supported")

  final def task = null
  final def isDataParallel = false
  final def size = 0
  final def cost = 0
  def nested = null

}