package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.cost._
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.{Compilers,CCompile}

/**
 * Author: Kevin J. Brown
 * Date: Dec 3, 2010
 * Time: 11:49:23 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * The base class of all static / walk-time schedulers
 * Defines the public interface for the rest of the Delite Runtime
 */

trait StaticScheduler {
  this: AbstractCostModel =>
	
  def schedule(graph: DeliteTaskGraph)

  protected def scheduleFlat(graph: DeliteTaskGraph)

  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule)

  protected def scheduleSequential(graph: DeliteTaskGraph)

  protected def enqueueRoots(graph: DeliteTaskGraph, opQueue: OpList) {
    for (op <- graph.ops) {
      if (!op.isSchedulable) {//if not already in opQueue (protects against same consumer appearing in list multiple times)
        op.processSchedulable
        if (op.isSchedulable) opQueue.add(op)
      }
    }
  }

  protected def split(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resourceList: Seq[Int]) {
    val header = OpHelper.expand(op, resourceList.length, graph, OpHelper.scheduledTarget(resourceList(0)))
    scheduleOn(header, schedule, resourceList(0)) //pick a resource out of the list to do the header

    val chunks = OpHelper.split(op, resourceList.length, graph, OpHelper.scheduledTarget(resourceList(0)))
    for ((resource, idx) <- resourceList zip (0 until resourceList.length)) {
      scheduleOn(chunks(idx), schedule, resource)
    }
  }

	protected def addSequential(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resource: Int) {
		op match {
			case c: OP_Condition => {
				scheduleSequential(c.predicateGraph)
				scheduleSequential(c.thenGraph)
				scheduleSequential(c.elseGraph)				
				splitNotEmpty(c, graph, schedule, List(c.predicateGraph.schedule, c.thenGraph.schedule, c.elseGraph.schedule), Seq(resource))			
			}
			case w: OP_While => {
				scheduleSequential(w.predicateGraph)
				scheduleSequential(w.bodyGraph)
				splitNotEmpty(w, graph, schedule, List(w.predicateGraph.schedule, w.bodyGraph.schedule), Seq(resource))			
			}
			case op if op.isDataParallel => split(op, graph, schedule, Seq(resource))
			case op => scheduleOn(op, schedule, resource)
		}		
	}
	
  protected def addNested(op: OP_Nested, graph: DeliteTaskGraph, schedule: PartialSchedule, resourceList: Seq[Int]) {
    op match {
      case c: OP_Condition => {
        scheduleFlat(c.predicateGraph)
        scheduleFlat(c.thenGraph)
        scheduleFlat(c.elseGraph)
        splitNotEmpty(c, graph, schedule, List(c.predicateGraph.schedule, c.thenGraph.schedule, c.elseGraph.schedule), resourceList)
      }
      case w: OP_While => {
				if (shouldParallelize(w, Map[String,Int]())){
					scheduleFlat(w.predicateGraph)	        
        	scheduleFlat(w.bodyGraph)
				}
				else {					
					scheduleSequential(w.predicateGraph)
					scheduleSequential(w.bodyGraph)
			  }        
				splitNotEmpty(w, graph, schedule, List(w.predicateGraph.schedule, w.bodyGraph.schedule), resourceList)			
      }
      case err => error("Control OP type not recognized: " + err.getClass.getSimpleName)
    }
  }

  protected def scheduleOn(op: DeliteOP, schedule: PartialSchedule, resource: Int) {
    //(op,Compilers(OpHelper.scheduledTarget(resource))) match {
    //  case (o:OP_Single,c:CCompile) => c.addKernel(op.id)
    //  case (o:OP_External,c:CCompile) => c.addKernel(op.id)
    //  case _ => //
    //}
    schedule(resource).add(op)
    op.scheduledResource = resource
    op.isSchedulable = true
    op.isScheduled = true
  }

  protected def splitNotEmpty(op: OP_Nested, graph: DeliteTaskGraph, outerSchedule: PartialSchedule, innerSchedules: List[PartialSchedule], resourceList: Seq[Int]) {
    val filteredList = resourceList.filter(i => innerSchedules.map(_(i).isEmpty) contains false)
    val chunkList = if (filteredList.isEmpty) Seq(resourceList(0)) else filteredList
    val chunks = op.makeChunks(chunkList, graph)

    val chunksIter = chunks.iterator
    for (i <- chunkList) {
      val chunk = chunksIter.next
      scheduleOn(chunk, outerSchedule, i)
    }
  }

  protected def ensureScheduled(graph: DeliteTaskGraph) {
    for (op <- graph.ops) {
      if (!op.isScheduled)
        error("Graph dependencies are unsatisfiable")
    }
  }


  //TODO: Separate hardware and programming model
  protected def scheduleOnGPU(op:DeliteOP) = {
    if (Config.numCuda + Config.numOpenCL == 0) false
    else if (Config.gpuWhiteList.size > 0) { // If white-list exists, then only white-list ops are scheduled on GPU
      if (Config.gpuWhiteList.contains(op.id)) true
      else false
    }
    else { // Otherwise, all the ops except black-list ops are scheduled on GPU
      if (Config.gpuBlackList.contains(op.id))
        false
      else if (!op.supportsTarget(Targets.Cuda) && !op.supportsTarget(Targets.OpenCL))
        false
      else
        true
    }
  }

}
