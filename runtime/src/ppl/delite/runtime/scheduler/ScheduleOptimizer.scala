package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph._
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets
import scala.collection.mutable.{HashMap,HashSet,ListBuffer}

trait ScheduleOptimizer {

  // Schedules C++ kernels on GPU host, removing Scala kernel dependency from GPU execution
  // TODO: Needs to consider executions with multiple CUDA or C++ schedules
  def scheduleNativeGPU(graph: DeliteTaskGraph) {
    
    assert(Config.numThreads == 1 && Config.numCuda == 1 && Config.scheduler=="dynamic")
    
    // a new schedule to be filled in
    val schedule = PartialSchedule(graph.schedule.numResources)
    
    // map from from op to sender op
    val senders = HashMap[(DeliteOP,Class[_]),Send]()

    val sortedNestedOps = ListBuffer[OP_Nested]()

    // for scala target, keep the original (ideally there should be no scala kernels scheduled)
    // merge two sends from an op that goes to both C++ and Cuda

    for (opList <- graph.schedule if OpHelper.scheduledTarget(opList.resourceID) == Targets.Scala) {
      for (op <- opList) {
        op match {
          case s: Send if senders.contains((s.from,s.getClass)) => // combine receivers to the same sender
            senders((s.from,s.getClass)).receivers ++= s.receivers
          case s: Send =>
            senders += (s.from,s.getClass) -> s
            schedule(opList.resourceID).add(op)
          case n: OP_Nested =>
            sortedNestedOps.append(n)
            schedule(opList.resourceID).add(op)
          case _ =>
            schedule(opList.resourceID).add(op)
        }
      }
      if (opList.size > 0)
        println("WARNING (scheduleNativeGPU): Configured to run GPU natively, but still " + opList.size + " scala kernels exist.")
    }
    
    // all ops in the current graph
    val ops = graph.schedule.resources.flatMap(_.toArray)

    // track additional dependencies for sync and free ops
    val deps = HashMap[DeliteOP, List[DeliteOP]]()
    def addDependency(from: DeliteOP, to: DeliteOP) {
      if (deps.contains(from))
        deps += from -> (deps(from) ++ List(to))
      else
        deps += from -> List(to)
    }
    def getDependency(from: DeliteOP): List[DeliteOP] = {
      if (deps.contains(from)) deps(from)
      else Nil
    }

    ops.foreach { _ match {
      case s: Send =>
        addDependency(s.from, s)
        for (r <- s.receivers) addDependency(s,r)
      case r: Receive if !r.sender.from.getDependencies.contains(r.to) =>
        addDependency(r, r.to)
      case f: Free =>
        for(i <- f.items if deps.contains(i._1)) {
          for (d <- deps(i._1).filter(_.isInstanceOf[Send]))
            addDependency(d, f)
        }
        addDependency(f.op, f)
      case _ => //
    } }

    // check if two nested ops are siblings (FIXME: Bettwer way to check?)
    def isSibling(op1: DeliteOP, op2: DeliteOP): Boolean = (op1,op2) match {
      case (n1: OP_Nested, n2: OP_Nested) => n1.id.split("_").head == n2.id.split("_").head
      case _ => false
    }

    // Add additional dependencies for nested ops.
    // Two nested ops without dependencies between the two may be reordered by SCC,
    // but they should keep the same order as in the Scala schedule to avoid deadlock from the inner schedule synchronizations.
    // Force the original ordering by adding extra dependencies.
    def serializeNestedOps(ns: List[OP_Nested]): Unit = ns match {
      case n1::n2::t =>
        val s1 = ops.filter(isSibling(_,n1))
        val s2 = ops.filter(isSibling(_,n2))
        for(from <- s1; to <- s2) addDependency(from, to)
        serializeNestedOps(n2::t)
      case _ =>
    }
    serializeNestedOps(sortedNestedOps.filter(n => ops.exists(o => isSibling(n,o))).toList)

    // Add additional dependencies to follow the same order as in the original schedule.
    // SCC reodering the original schedule could result in memory management issue (Free node scheduled too early)
    // since Free nodes are added based on the original schedule.
    // Example: x5 and x6 both with input x1 and no dependencies between the two. x1 is freed after x6 but x5/x6 could be reordered.
    // Also could be more precise by following the consumers but this is simpler.
    def serializeOps(ns: List[DeliteOP]): Unit = ns match {
      case n1::n2::t =>
        addDependency(n1,n2)
        serializeOps(n2::t)
      case _ =>
    }
    for (s <- graph.schedule.resources) serializeOps(s.toArray.toList)

    // topological sort on the original schedule
    val sortedSchedule = GraphUtil.stronglyConnectedComponents[DeliteOP](ops.toList,
                         op => op.getConsumers.toList ++ getDependency(op)).flatMap(l => l.filterNot(o => o.scheduledOn(Targets.Scala)))

    val combinedResource = schedule(Targets.resourceIDs(Targets.Cuda).head)

    val nests = HashSet[DeliteOP]()

    sortedSchedule.foreach { _ match {
      case nested: OP_Nested if nests.contains(nested) => // process only once for each nested op
      case nested: OP_Nested => 
        // recursively schedule nested graphs
        nested.nestedGraphs.foreach(scheduleNativeGPU)

        // check if a sibling of this nested OP exists on CUDA schedule
        val siblings = sortedSchedule.filter(op => (op.id.split("_").head == nested.id.split("_").head) && (op != nested))
        assert(siblings.size <= 1)
        
        // combine the siblings of a nested op 
        if (siblings.size > 0) {
          val nested2 = siblings.head

          // returner is the one that exists in the graph (the other one is phantom)
          val returner = if (graph.ops.contains(nested)) nested else nested2
          
          // recover the dependencies of the returner (lost during makeChunks call)
          for (in <- (nested.getInputs ++ nested2.getInputs).distinct if (!returner.getInputs.contains(in))) {
            returner.addInput(in._1, in._2)
          }
          for (in <- (nested.getMutableInputs ++ nested2.getMutableInputs)) {
            returner.addMutableInput(in._1, in._2)
          }
          for (in <- (nested.getAntiDeps ++ nested2.getAntiDeps)) {
            returner.addAntiDep(in)
          }
          combinedResource.add(returner)
          nests.add(nested)
          nests.add(nested2)
        }
        else {
         combinedResource.add(nested) 
        }
      case op => combinedResource.add(op)  
    } }

    // update the graph schedule
    graph.schedule = schedule
  }

}
