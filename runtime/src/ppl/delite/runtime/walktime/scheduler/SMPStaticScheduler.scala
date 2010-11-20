package ppl.delite.runtime.walktime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.walktime.codegen.{ExecutableGenerator, DeliteExecutable}
import ppl.delite.runtime.walktime.codegen.kernels.scala.{Reduce_SMP_Array_Generator, Map_SMP_Array_Generator}
import ppl.delite.runtime.walktime.graph.ops.{OP_Reduce, OP_Map, DeliteOP}
import ppl.delite.runtime.walktime.graph.DeliteTaskGraph
import java.util.ArrayDeque

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
      split(op) //split and schedule op across all threads
    }
    else {
      //look for best place to put this op (simple nearest-neighbor clustering)
      var i = 0
      var notDone = true
      val deps = op.getDependencies
      while (i < numThreads && notDone) {
        if (deps.contains(procs(i).peekLast)) {
          procs(i).add(op)
          op.scheduledResource = i
          notDone = false
          if (nextThread == i) nextThread = (nextThread + 1) % numThreads
        }
        i += 1
      }
      //else submit op to next thread in the rotation (round-robin)
      if (notDone) {
        procs(nextThread).add(op)
        op.scheduledResource = nextThread
        nextThread = (nextThread + 1) % numThreads
      }
      op.isScheduled = true
    }
  }

  private def enqueueRoots(graph: DeliteTaskGraph) {
    val end = graph.result
    traverse(end)
  }

  private def traverse(op: DeliteOP) {
    if (!op.isSchedulable) { //not already in opQueue
      op.processSchedulable
      if (op.isSchedulable) opQueue.add(op)
    }
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

  private def split(op: DeliteOP) {
    op match { //NOTE: match on OP type since different data parallel ops can have different semantics / scheduling implications
      case map: OP_Map => {
        for (i <- 0 until numThreads) {
          val chunk = Map_SMP_Array_Generator.makeChunk(map, i, numThreads)
          procs(i).add(chunk)
          chunk.isScheduled = true
          chunk.scheduledResource = i
        }
      }
      case reduce: OP_Reduce[_] => {
        for (i <- 0 until numThreads) {
          val chunk = Reduce_SMP_Array_Generator.makeChunk(reduce, i, numThreads)
          procs(i).add(chunk)
          chunk.isScheduled = true
          chunk.scheduledResource = i
        }
      }
      case other => error("OP type not recognized: " + other.getClass.getSimpleName)
    }
  }


  private def processScheduledWork: Array[ArrayDeque[DeliteExecutable]] = {
    val queues = new Array[ArrayDeque[DeliteExecutable]](numThreads)
    //generate executable(s) for all the ops in each proc
    val executables = ExecutableGenerator.makeExecutables(procs)

    for (i <- 0 until numThreads) {
      queues(i) = new ArrayDeque[DeliteExecutable]
      queues(i).add(executables(i))
    }
    queues
  }

  private def createStaticSchedule(queues: Array[ArrayDeque[DeliteExecutable]]): StaticSchedule = {
    new StaticSchedule(queues)
  }

}