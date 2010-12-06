package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops.DeliteOP
import java.util.ArrayDeque
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 1:43:32 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * A simply static scheduler that mimics the classic CUDA programming model
 * All OPs that can be executed on the GPU do so
 * The remaining ops execute in a single CPU thread
 */
final class GPUOnlyStaticScheduler extends StaticScheduler {

  private val gpuResource = new ArrayDeque[DeliteOP] //one GPU stream
  private val cpuResource = new ArrayDeque[DeliteOP] //one CPU thread

  private val opQueue = new ArrayDeque[DeliteOP]

  def schedule(graph: DeliteTaskGraph): PartialSchedule = {
    assert(Config.numThreads == 1 && Config.numGPUs == 1)
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
    if (op.supportsTarget(Targets.Cuda)) { //schedule on GPU resource
      gpuResource.add(op)
      op.scheduledResource = 1
    }
    else { //schedule on CPU resource
      cpuResource.add(op)
      op.scheduledResource = 0
    }
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
    new PartialSchedule(Array[ArrayDeque[DeliteOP]](cpuResource, gpuResource))
  }

}
