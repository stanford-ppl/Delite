package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops.DeliteOP
import java.util.ArrayDeque

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 1:43:32 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

final class GPUOnlyStaticScheduler extends StaticScheduler {

  private val resource = new ArrayDeque[DeliteOP]

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

  private def scheduleOne(op: DeliteOP) {
    resource.add(op)
    op.scheduledResource = 0
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
    new PartialSchedule(Array[ArrayDeque[DeliteOP]](resource))
  }

}
