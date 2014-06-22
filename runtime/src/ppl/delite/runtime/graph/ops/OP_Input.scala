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
 */
class OP_Input(val op: DeliteOP) extends DeliteOP {

  def id = "input_" + op.id

  private[graph] var outputTypesMap = op.outputTypesMap
  private[graph] override val stencilMap = op.stencilMap

  override def partition = op.partition
  override def partition(symbol: String) = op.partition(symbol)

  isSchedulable = true
  isScheduled = true

  def task = null
  def isDataParallel = false
  def cost = 0
  def size = 0

  override def toString = id

}
