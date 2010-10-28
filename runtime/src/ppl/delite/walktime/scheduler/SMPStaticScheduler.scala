package ppl.delite.walktime.scheduler

import ppl.delite.walktime.graph.DeliteTaskGraph
import ppl.delite.io.Config
import ppl.delite.walktime.graph.ops.DeliteOP
import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}
import ppl.delite.walktime.codegen.{SingleOPExecutable, DeliteExecutable}
import java.util.{HashMap, ArrayDeque}

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

final class SMPStaticScheduler {

  private val numThreads = Config.numThreads

  private val procs = new Array[ArrayDeque[DeliteOP]](numThreads)
  for (i <- 0 until numThreads) procs(i) = new ArrayDeque[DeliteOP]

  private val opQueue = new ArrayDeque[DeliteOP]

  def schedule(graph: DeliteTaskGraph) : StaticSchedule = {
    //traverse nesting & schedule sub-graphs
    //TODO: implement functionality for nested graphs
    scheduleFlat(graph)

    //code gen
    val queues = processScheduledWork

    //return schedule
    createStaticSchedule(queues)
  }

  private def scheduleFlat(graph: DeliteTaskGraph) {
    enqueueRoots(graph)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      scheduleOne(op)
      processConsumers(op)
    }
  }

  //NOTE: this is currently the simple scheduler from Delite 1.0
  var nextThread = 0

  private def scheduleOne(op: DeliteOP) {
    if (op.isDataParallel) {
      //split op across all available threads
      for (proc <- procs) proc.add(op)
    }
    else {
      //look for best place to put this op (simple nearest-neighbor clustering clustering)
      var i = 0
      var notDone = true
      val deps = op.getDependencies
      while (i < numThreads && notDone) {
        if (deps.contains(procs(i).peekLast)) {
          procs(i).add(op)
          notDone = false
        }
        i += 1
      }
      //else submit op to next thread in the rotation (round-robin)
      if (notDone) {
        procs(nextThread).add(op)
        nextThread = (nextThread + 1) % numThreads
      }
    }
    op.isScheduled = true
  }

  private def enqueueRoots(graph: DeliteTaskGraph) {
    val end = graph.root
    traverse(end)
  }

  private def traverse(op: DeliteOP) {
    op.processSchedulable
    if (op.isSchedulable) opQueue.add(op)
    for (dep <- op.getDependencies) {
      traverse(dep)
    }
  }

  private def processConsumers(op: DeliteOP) {
    for (c <- op.getConsumers) {
      c.processSchedulable
      if (c.isSchedulable) opQueue.add(c)
    }
  }

  private def processScheduledWork: Array[ArrayDeque[DeliteExecutable]] = {
    val queues = new Array[ArrayDeque[DeliteExecutable]](numThreads)
    for (i <- 0 until numThreads) queues(i) = new ArrayDeque[DeliteExecutable]
    //generate executable(s) for all the ops in each proc
    createExecutables(procs, queues)
    queues
  }

  private def createStaticSchedule(queues: Array[ArrayDeque[DeliteExecutable]]): StaticSchedule = {
    new StaticSchedule(queues)
  }

  //TODO: these helper methods are hacks used until I add code generation with fusion
  //TODO: should refactor this out of scheduler and into codegen
  private def createExecutables(opQueues: Array[ArrayDeque[DeliteOP]], workQueues: Array[ArrayDeque[DeliteExecutable]]) {
    val table = new HashMap[DeliteOP, SingleOPExecutable[_]]
    //create all executables
    for (i <- 0 until numThreads) {
      val iter = opQueues(i).iterator
      while (iter.hasNext) {
        val op = iter.next
        val work = new SingleOPExecutable[Any](op.task)
        table.put(op, work)
      }
    }
    //add dependencies
    for (i <- 0 until numThreads) {
      val iter = opQueues(i).iterator
      while (iter.hasNext) {
        val op = iter.next
        val work = table.get(op)
        for (dep <- op.getDependencies) {
          work.addDependency(table.get(dep))
        }
        workQueues(i).add(work)
      }
    }
  }

  private def addExecutable(opQueue: ArrayDeque[DeliteOP], workQueue: ArrayDeque[DeliteExecutable]) {
    for (i <- 0 until opQueue.size) {
      val op = opQueue.poll
      val work = new SingleOPExecutable[Any](op.task)
      workQueue.add(work)
    }
  }

}