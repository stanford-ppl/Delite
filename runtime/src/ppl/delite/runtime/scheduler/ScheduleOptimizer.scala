package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph._
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets

trait ScheduleOptimizer {

  // Schedules C++ kernels on GPU host, removing Scala kernel dependency from GPU execution
  // TODO: Needs to consider executions with multiple CUDA or C++ schedules
  def scheduleNativeGPU(graph: DeliteTaskGraph) {
    
    assert(Config.numCuda == 1 && Config.scheduler=="dynamic")
    
    // a new schedule to be filled in
    val schedule = PartialSchedule(graph.schedule.numResources)
    
    // for scala target, keep the original (ideally there should be no scala kernels scheduled)
    for (resource <- graph.schedule if (OpHelper.scheduledTarget(resource.resourceID) == Targets.Scala)) {
      resource.foreach(op => schedule(resource.resourceID).add(op))
      if (resource.size > 0) println("WARNING (scheduleNativeGPU): Configured to run GPU natively, but still " + resource.size + " scala kernels exist.")
    }

    // FIXME: Is safe to assume that all C++ are scheduled at the head resource for dynamic scheduling?
    val hostResource = graph.schedule(Targets.resourceIDs(Targets.Cpp).head)
    val deviceResource = graph.schedule(Targets.resourceIDs(Targets.Cuda).head)
    val combinedResource = schedule(deviceResource.resourceID)
    
    var dIdx = 0

    // merge OPs in C++ schedule into GPU schedule with proper ordering
    hostResource.foreach { _ match {
      case recv: Receive if (recv.sender.from.scheduledOn(Targets.Cuda) && !combinedResource.contains(recv.sender)) =>
        // when the sender is from CUDA and not already emitted, emit up to the sender
        assert(deviceResource.contains(recv.sender))
        while (deviceResource(dIdx) != recv.sender) {
          combinedResource.add(deviceResource(dIdx))
          dIdx += 1
        }
        // add sender
        combinedResource.add(deviceResource(dIdx)); dIdx += 1;
        combinedResource.add(recv)
      case nested: OP_Nested => 
        // recursively schedule nested graphs
        nested.nestedGraphs.foreach(scheduleNativeGPU)

        // check if a sibling of this nested OP exists on CUDA schedule (FIXME: better way to check?)
        val siblings = deviceResource.toArray().filter(op => op.id.split("_").head == nested.id.split("_").head)
        assert(siblings.size <= 1)
        
        // combine the siblings of a nested op 
        if (siblings.size > 0) {
          val nested_dev = siblings.head
          
          // returner is the one that exists in the graph (the other one is phantom)
          val returner = if (graph.ops.contains(nested)) nested else nested_dev
          
          // recover the dependencies of the returner (lost during makeChunks call)
          for (in <- (nested.getInputs ++ nested_dev.getInputs).distinct if (!returner.getInputs.contains(in))) {
            returner.addInput(in._1, in._2)
          }
          for (in <- (nested.getMutableInputs ++ nested_dev.getMutableInputs)) {
            returner.addMutableInput(in._1, in._2)
          }
          for (in <- (nested.getAntiDeps ++ nested_dev.getAntiDeps)) {
            returner.addAntiDep(in)
          }
          
          // sync both schedules to the nested op
          while (deviceResource(dIdx) != nested_dev) {
            combinedResource.add(deviceResource(dIdx))
            dIdx += 1
          }
          
          if (returner == nested) { // only add nested (host op)
            combinedResource.add(nested)
            dIdx += 1   
          }
          else { // only add nested_dev (device op)
            combinedResource.add(deviceResource(dIdx))
            dIdx += 1
          }
        }
        else {
         combinedResource.add(nested) 
        }
      case op => combinedResource.add(op)  
    } }

    // emit remainder ops in the device schedule
    while (dIdx < deviceResource.size) {
      combinedResource.add(deviceResource(dIdx))
      dIdx += 1
    }
        
    // update the graph schedule
    graph.schedule = schedule
  }

}
