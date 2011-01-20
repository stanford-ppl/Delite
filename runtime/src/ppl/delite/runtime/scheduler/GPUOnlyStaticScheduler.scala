package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.DeliteTaskGraph
import java.util.ArrayDeque
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.kernels.scala._
import ppl.delite.runtime.graph.ops._

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

  def schedule(graph: DeliteTaskGraph): PartialSchedule = {
    assert(Config.numThreads == 1 && Config.numGPUs == 1)
    scheduleFlat(graph)
    ensureScheduled(graph)
    createPartialSchedule
  }

  private def scheduleFlat(graph: DeliteTaskGraph) {
    val opQueue = new ArrayDeque[DeliteOP]
    enqueueRoots(graph, opQueue)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      scheduleOne(op, graph)
      processConsumers(op, opQueue)
    }
  }

  private def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph) {
    op match {
      case c: OP_Control => addControl(c)
      case _ => {
        if (op.supportsTarget(Targets.Cuda)) { //schedule on GPU resource
          if (op.isDataParallel) {
            splitGPU(op)
          }
          else {
            gpuResource.add(op)
            op.scheduledResource = 1
          }
        }
        else if (op.variant != null) { //kernel could be partially GPUable
          OpHelper.makeVariant(op, graph)
          scheduleFlat(op.variant)
        }
        else { //schedule on CPU resource
          if (op.isDataParallel) {
            split(op, graph)
          }
          else {
            cpuResource.add(op)
            op.scheduledResource = 0
          }
        }
        op.isScheduled = true
      }
    }
  }

  private def enqueueRoots(graph: DeliteTaskGraph, opQueue: ArrayDeque[DeliteOP]) {
    for (op <- graph.ops) {
      op.processSchedulable
      if (op.isSchedulable) opQueue.add(op)
    }
  }

  private def processConsumers(op: DeliteOP, opQueue: ArrayDeque[DeliteOP]) {
    for (c <- op.getConsumers) {
      if (!c.isSchedulable) {//if not already in opQueue (protects against same consumer appearing in list multiple times)
        c.processSchedulable
        if (c.isSchedulable) opQueue.add(c)
      }
    }
  }

  private def split(op: DeliteOP, graph: DeliteTaskGraph) {
    val header = OpHelper.expand(op, 1, graph)
    cpuResource.add(header)
    header.scheduledResource = 0
    header.isScheduled = true

    val chunk = OpHelper.split(op, 0, 1, graph.kernelPath)
    cpuResource.add(chunk)
    chunk.scheduledResource = 0
    chunk.isScheduled = true
  }

  private def splitGPU(op: DeliteOP) {
    val chunk = OpHelper.splitGPU(op)
    gpuResource.add(chunk)
    chunk.scheduledResource = 1
    chunk.isScheduled = true
  }

  private def addControl(op: OP_Control) {
    val chunk0 = op.makeChunk(0)
    cpuResource.add(chunk0)
    chunk0.scheduledResource = 0
    chunk0.isScheduled = true

    val chunk1 = op.makeChunk(1)
    gpuResource.add(chunk1)
    chunk1.scheduledResource = 1
    chunk1.isScheduled = true
  }

  private def ensureScheduled(graph: DeliteTaskGraph) {
    for (op <- graph.ops) {
      if (!op.isScheduled)
        error("Graph dependencies are unsatisfiable")
    }
  }

  private def createPartialSchedule = {
    new PartialSchedule(Array[ArrayDeque[DeliteOP]](cpuResource, gpuResource))
  }

}
