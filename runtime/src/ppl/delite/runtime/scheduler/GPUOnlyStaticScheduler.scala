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
      if (op.isDataParallel) {
        split(op)
      }
      else {
        cpuResource.add(op)
        op.scheduledResource = 0
        op.isScheduled = true
      }
    }
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

  //TODO: since codegen of data parallel ops is non-optional, some of this should be factored out of the different schedulers
  private def split(op: DeliteOP) {
    val chunk = op match { //NOTE: match on OP type since different data parallel ops can have different semantics / scheduling implications
      case map: OP_Map => Map_SMP_Array_Generator.makeChunk(map, 0, 1)
      case reduce: OP_Reduce => Reduce_SMP_Array_Generator.makeChunk(reduce, 0, 1)
      case mapReduce: OP_MapReduce => MapReduce_SMP_Array_Generator.makeChunk(mapReduce, 0, 1)
      case other => error("OP type not recognized: " + other.getClass.getSimpleName)
    }
    cpuResource.add(chunk)
    chunk.scheduledResource = 0
    chunk.isScheduled = true
  }

  private def createPartialSchedule = {
    new PartialSchedule(Array[ArrayDeque[DeliteOP]](cpuResource, gpuResource))
  }

}
