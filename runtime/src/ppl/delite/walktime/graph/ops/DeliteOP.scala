package ppl.delite.walktime.graph.ops

import ppl.delite.walktime.graph.DeliteTaskGraph

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:33:29 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteOP {

  def task : Unit

  def getDependencies : Seq[DeliteOP]

  def getConsumers : Seq[DeliteOP]

  def nested : DeliteTaskGraph

  def cost: Int

  def size: Int

  def isDataParallel : Boolean

  var isSchedulable = false

  var isScheduled = false

  def processSchedulable {
    var free = true
    for (dep <- getDependencies) {
      free &&= dep.isScheduled    
    }
    if (free) isSchedulable = true
  }

}