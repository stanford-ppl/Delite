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

  /**
   * these methods should be instantiated from parsing the Delite Execution Graph input
   */
  def task : String

  def outputType: String

  def getDependencies : Seq[DeliteOP]

  def getConsumers : Seq[DeliteOP]

  //this is a subset of getDependencies and contains the inputs in the order required to call the task
  def getInputs : Seq[DeliteOP]

  def nested : DeliteTaskGraph

  def cost: Int

  def size: Int

  def isDataParallel : Boolean

  /**
   * these methods/state are used for scheduling
   */
  var isSchedulable = false

  var isScheduled = false

  def processSchedulable {
    var free = true
    for (dep <- getDependencies) {
      free &&= dep.isScheduled    
    }
    if (free) isSchedulable = true
  }

  /**
   * these methods/state are used for code generation
   */
  var scheduledResource = -1

  def id: Int = {
    System.identityHashCode(this)
  }
}