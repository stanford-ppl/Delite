package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph._
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets
import scala.collection.mutable.{HashMap,HashSet}

trait ScheduleOptimizer {

  // Schedules C++ kernels on GPU host, removing Scala kernel dependency from GPU execution
  // TODO: Needs to consider executions with multiple CUDA or C++ schedules
  def scheduleNativeGPU(graph: DeliteTaskGraph) {
    
    assert(Config.numThreads == 1 && Config.numCuda == 1 && Config.scheduler=="dynamic")
    
    // a new schedule to be filled in
    val schedule = PartialSchedule(graph.schedule.numResources)
    
    // map from from op to sender op
    val senders = HashMap[DeliteOP,Send]()

    // for scala target, keep the original (ideally there should be no scala kernels scheduled)
    // merge two sends from an op that goes to both C++ and Cuda
    for (opList <- graph.schedule if OpHelper.scheduledTarget(opList.resourceID) == Targets.Scala) {
      for (op <- opList) {
        op match {
          case s: Send if senders.contains(s.from) => // combine receivers to the same sender
            senders(s.from).receivers ++= s.receivers
          case s: Send =>
            senders += s.from -> s
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

        // check if a sibling of this nested OP exists on CUDA schedule (FIXME: better way to check?)
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
