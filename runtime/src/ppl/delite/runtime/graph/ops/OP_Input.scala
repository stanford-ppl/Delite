package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 8:36 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * An input to a DeliteTaskGraph
 * Used to register OPs' dependencies on the input to the (sub)graph
 * Mirrors the id and output information of a real OP in some outer scope
 */

class OP_Input(op: DeliteOP) extends DeliteOP {

  def id = op.id
  def supportsTarget(target: Targets.Value) = op.supportsTarget(target)
  def outputType(target: Targets.Value) = op.outputType(target)
  override def outputType = outputType(Targets.Scala)

  isSchedulable = true
  isScheduled = true

  def task = null
  def isDataParallel = false
  def cost = 0
  def size = 0

}
