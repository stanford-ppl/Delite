/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops.{OP_Nested, DeliteOP}
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.cost._

/**
 * @author Kevin J. Brown
 */

final class SMP_GPU_StaticScheduler extends StaticScheduler with ParallelUtilizationCostModel {

  private val numCPUs = Config.numThreads
  private val numGPUs = Config.numGPUs
  private val gpu = numCPUs

  def schedule(graph: DeliteTaskGraph) {
    assert(numGPUs <= 1)
    //traverse nesting & schedule sub-graphs, starting with outermost graph
    scheduleFlat(graph)
  }

  protected def scheduleSequential(graph: DeliteTaskGraph) = scheduleFlat(graph, true)

  protected def scheduleFlat(graph: DeliteTaskGraph) = scheduleFlat(graph, false)

  protected def scheduleFlat(graph: DeliteTaskGraph, sequential: Boolean) {
    val opQueue = new ArrayDeque[DeliteOP]
    val schedule = PartialSchedule(numCPUs + numGPUs)
    enqueueRoots(graph, opQueue)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      if (sequential)
        addSequential(op, graph, schedule, 0)
      else
        scheduleOne(op, graph, schedule)
      enqueueRoots(graph, opQueue)
    }
    ensureScheduled(graph)
    graph.schedule = schedule
  }

  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    op match {
      case c: OP_Nested => addNested(c, graph, schedule, Range(0, numCPUs + numGPUs))
      case _ => {
        if (op.supportsTarget(Targets.Cuda) && numGPUs > 0) { //schedule on GPU resource
          if (op.isDataParallel)
            splitGPU(op, schedule)
          else {
            scheduleOn(op, schedule, gpu)
          }
        }
        else if (op.variant != null && numGPUs > 0) { //kernel could be partially GPUable
          addNested(op.variant, graph, schedule, Range(0, numCPUs + numGPUs))
        }
        else { //schedule on CPU resource
          if (op.isDataParallel)
            split(op, graph, schedule, Range(0, numCPUs))
          else
            cluster(op, schedule)
        }
      }
    }
  }

  private var nextThread = 0

  private def cluster(op: DeliteOP, schedule: PartialSchedule) {
    //look for best place to put this op (simple nearest-neighbor clustering)
    var i = 0
    var notDone = true
    val deps = op.getDependencies
    while (i < numCPUs && notDone) {
      if (deps.contains(schedule(i).peekLast)) {
        scheduleOn(op, schedule, i)
        notDone = false
        if (nextThread == i) nextThread = (nextThread + 1) % numCPUs
      }
      i += 1
    }
    //else submit op to next thread in the rotation (round-robin)
    if (notDone) {
      scheduleOn(op, schedule, nextThread)
      nextThread = (nextThread + 1) % numCPUs
    }
  }

  private def splitGPU(op: DeliteOP, schedule: PartialSchedule) {
    val chunk = OpHelper.splitGPU(op)
    scheduleOn(chunk, schedule, gpu)
  }

}
