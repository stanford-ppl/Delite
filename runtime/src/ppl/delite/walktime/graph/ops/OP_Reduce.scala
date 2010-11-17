package ppl.delite.walktime.graph.ops

import ppl.delite.data.Data

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:27:00 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Reduce extends DeliteOP {

  final def isDataParallel = true

  def task = "println"

  def outputType = "String" //TODO: this a placeholder; probably want Manifests

  /**
   * Since the semantics of Reduce are to return an A, all chunks are necessarily complete before the final A can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk: OP_Reduce = {
    val r = new OP_Reduce
    r.dependencyList = dependencyList //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def nested = null
  def cost = 0
  def size = 0

}