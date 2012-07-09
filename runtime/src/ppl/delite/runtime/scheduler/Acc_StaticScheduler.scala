package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops.{OP_Nested, DeliteOP}
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.cost._


final class Acc_StaticScheduler extends StaticScheduler with ParallelUtilizationCostModel {

  private val numCPUs = Config.numThreads
  private val numAccs = Config.numCpp + Config.numCuda + Config.numOpenCL

  def schedule(graph: DeliteTaskGraph) {
    //assert(numAccs <= 1)
    //traverse nesting & schedule sub-graphs, starting with outermost graph
    scheduleFlat(graph)
  }

  protected def scheduleSequential(graph: DeliteTaskGraph) = scheduleFlat(graph, true)

  protected def scheduleFlat(graph: DeliteTaskGraph) = scheduleFlat(graph, false)

  protected def scheduleFlat(graph: DeliteTaskGraph, sequential: Boolean) {
    assert(numAccs > 0)
    val opQueue = new OpList
    val schedule = PartialSchedule(numCPUs + numAccs)
    enqueueRoots(graph, opQueue)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      if (sequential)
        addSequential(op, graph, schedule, numCPUs)
      else
        scheduleOne(op, graph, schedule)
      enqueueRoots(graph, opQueue)
    }
    ensureScheduled(graph)
    graph.schedule = schedule
  }

  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    op match {
      case c: OP_Nested => addNested(c, graph, schedule, Range(0, numCPUs + numAccs))
      case _ => {
        if (scheduleOnAcc(op)) {
          if (op.isDataParallel)
            splitAcc(op, schedule)
          else {
            roundrobin(op, schedule)
          }
        }
        else if (op.variant != null && numAccs > 0) {
          addNested(op.variant, graph, schedule, Range(0, numCPUs + numAccs))
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

  private var rrIdx = 0
  private def roundrobin(op: DeliteOP, schedule: PartialSchedule) {
    scheduleOn(op, schedule, numCPUs + rrIdx)
    rrIdx = (rrIdx + 1) % numAccs
  }

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

  override protected def split(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resourceList: Seq[Int]) {
    if(resourceList == Seq(numCPUs)) splitAcc(op, schedule)
    else super.split(op, graph, schedule, resourceList)
  }
  
  private def splitAcc(op: DeliteOP, schedule: PartialSchedule) {
    assert(false, "data-parallel op is not supported yet for native ops")
    op.scheduledResource = numCPUs // TODO: Check if this is okay (Need to set this because MultiLoop GPU generator needs to know the resource ID of this op)
    val chunk = OpHelper.splitAcc(op)
    scheduleOn(chunk, schedule, numCPUs)
  }

  private def scheduleOnAcc(op:DeliteOP) = {
    if (!op.supportsTarget(Targets.Cuda) && !op.supportsTarget(Targets.OpenCL) && !op.supportsTarget(Targets.Cpp))
      false
    else
      true
  }

}
