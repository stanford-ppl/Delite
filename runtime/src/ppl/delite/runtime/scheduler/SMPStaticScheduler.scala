package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.cost._

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:02:57 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * A completely static scheduler for an SMP system
 *
 * @author Kevin J. Brown
 *
 */

final class SMPStaticScheduler extends StaticScheduler with ParallelUtilizationCostModel {

  private val numThreads = Config.numThreads

  def schedule(graph: DeliteTaskGraph) {
    //traverse nesting & schedule sub-graphs, starting with outermost graph
    scheduleFlat(graph)
  }

  protected def scheduleSequential(graph: DeliteTaskGraph) = scheduleFlat(graph, true)

  protected def scheduleFlat(graph: DeliteTaskGraph) = scheduleFlat(graph, false)

  protected def scheduleFlat(graph: DeliteTaskGraph, sequential: Boolean) {
    val opQueue = new ArrayDeque[DeliteOP]
    val schedule = PartialSchedule(numThreads)
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

  //NOTE: this is currently the simple scheduler from Delite 1.0
  var nextThread = 0 //TODO: this isn't reset across nested graphs, but does it really matter?

  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    op match {
      case c: OP_Nested => addNested(c, graph, schedule, Range(0, numThreads))
			case l: OP_MultiLoop => 
				if (shouldParallelize(l, Map[String,Int]())){
					split(op, graph, schedule, Range(0, numThreads))
				}
				else {
					split(op, graph, schedule, Seq(0))
				} 
      case _ => {
        //if (op.variant != null) addNested(op.variant, graph, schedule, Range(0, numThreads)) else
        if (op.isDataParallel) split(op, graph, schedule, Range(0, numThreads))
        else cluster(op, schedule)
      }
    }
  }

  private def cluster(op: DeliteOP, schedule: PartialSchedule) {
    //look for best place to put this op (simple nearest-neighbor clustering)
    var i = 0
    var notDone = true
    val deps = op.getDependencies
    while (i < numThreads && notDone) {
      if (deps.contains(schedule(i).peekLast)) {
        scheduleOn(op, schedule, i)
        notDone = false
        if (nextThread == i) nextThread = (nextThread + 1) % numThreads
      }
      i += 1
    }
    //else submit op to next thread in the rotation (round-robin)
    if (notDone) {
      scheduleOn(op, schedule, nextThread)
      nextThread = (nextThread + 1) % numThreads
    }
  }

}
