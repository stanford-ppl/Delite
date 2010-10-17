package ppl.delite.walktime.scheduler

import ppl.delite.walktime.graph.DeliteTaskGraph
import ppl.delite.io.Config
import ppl.delite.walktime.graph.ops.DeliteOP
import java.util.ArrayDeque
import ppl.delite.walktime.codegen.DeliteExecutable

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

class SMPStaticScheduler {

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
  var lastOP: DeliteOP = _

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
    //generate executable for all the ops in each proc
    //TODO: perform code generation / fusion here
    for (i <- 0 until numThreads) {
      for (j <- 0 until procs(i).size) {
        val work = new DeliteExecutable{def task = println("Hello World")} //TODO: how do we link kernels here?
        queues(i).add(work)
      }
    }
    queues
  }

  private def createStaticSchedule(queues: Array[ArrayDeque[DeliteExecutable]]): StaticSchedule = {
    new StaticSchedule(queues)
  }

}