package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.graph.DeliteTaskGraph
import util.Random

/**
 * Author: Kevin J. Brown
 * Date: Dec 3, 2010
 * Time: 11:44:57 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

final class DieRollStaticScheduler extends StaticScheduler {

  private val numCPUThreads = Config.numThreads
  private val numGPUs = Config.numGPUs

  private val numResources = numCPUThreads + numGPUs

  private val resources = new Array[ArrayDeque[DeliteOP]](numResources)
  for (i <- 0 until numResources) resources(i) = new ArrayDeque[DeliteOP]

  private val opQueue = new ArrayDeque[DeliteOP]

  def schedule(graph: DeliteTaskGraph): PartialSchedule = {
    scheduleFlat(graph)
    createPartialSchedule
  }

  private def scheduleFlat(graph: DeliteTaskGraph) {
    enqueueRoots(graph)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      scheduleOne(op)
      processConsumers(op)
    }
  }

  private val die = new Random()

  private def nextResource = {
    die.nextInt(numResources)
  }

  private def scheduleOne(op: DeliteOP) {
    val resource = nextResource
    resources(resource).add(op)
    op.scheduledResource = resource
    op.isScheduled = true
  }

  private def enqueueRoots(graph: DeliteTaskGraph) {
    for (op <- graph.ops) {
      op.processSchedulable
      if (op.isSchedulable) opQueue.add(op)
    }
  }

  private def processConsumers(op: DeliteOP) {
    for (c <- op.getConsumers) {
      if (!c.isSchedulable) {//if not already in opQueue (protects against same consumer appearing in list multiple times)
        c.processSchedulable
        if (c.isSchedulable) opQueue.add(c)
      }
    }
  }

  private def createPartialSchedule = {
    new PartialSchedule(resources)
  }

}
