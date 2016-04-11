package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.cost._
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.{Compilers,CCompile}

trait StaticScheduler {
  this: AbstractCostModel =>
	
  def schedule(graph: DeliteTaskGraph)

  protected def scheduleFlat(graph: DeliteTaskGraph)

  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule)

  protected def scheduleSequential(graph: DeliteTaskGraph, resource: Int)

  protected def enqueueRoots(graph: DeliteTaskGraph, opQueue: OpList) {
    for (op <- graph.ops) {
      if (!op.isSchedulable) {//if not already in opQueue (protects against same consumer appearing in list multiple times)
        op.processSchedulable
        if (op.isSchedulable) opQueue.add(op)
      }
    }
  }

  protected def split(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resourceList: Seq[Int]) {
    // val header = OpHelper.expand(op, resourceList.length, graph, OpHelper.scheduledTarget(resourceList(0)))
    // scheduleOn(header, schedule, resourceList(0)) //pick a resource out of the list to do the header

    // val chunks = OpHelper.split(op, resourceList.length, graph, OpHelper.scheduledTarget(resourceList(0)))
    // for ((resource, idx) <- resourceList zip (0 until resourceList.length)) {
    //   scheduleOn(chunks(idx), schedule, resource)
    // }
    scheduleOn(op, schedule, resourceList(0))
  }

  /**
   * [COMMENT TODO] What does this method do? It seems to be used like the 'scheduleOn' method. Why do we need this?
   * @param op:
   * @param graph:
   * @param schedule:
   * @param resource:
   */
	protected def addSequential(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resource: Int) {
		op match {
			case c: OP_Condition => {
				scheduleSequential(c.predicateGraph, resource)
				scheduleSequential(c.thenGraph, resource)
				scheduleSequential(c.elseGraph, resource)
				splitNotEmpty(c, graph, schedule, List(c.predicateGraph.schedule, c.thenGraph.schedule, c.elseGraph.schedule), Seq(resource))			
			}
			case w: OP_While => {
				scheduleSequential(w.predicateGraph, resource)
				scheduleSequential(w.bodyGraph, resource)
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
					scheduleSequential(w.predicateGraph, 0) //TODO: shouldn't assume this
					scheduleSequential(w.bodyGraph, 0)
			  }        
				splitNotEmpty(w, graph, schedule, List(w.predicateGraph.schedule, w.bodyGraph.schedule), resourceList)			
      }
      case err => error("Control OP type not recognized: " + err.getClass.getSimpleName)
    }
  }

  /**
   * Schedule a given DeliteOP on the given resource ID (thread ID) and update the contents
   * of 'schedule'.
   * @param op: [[DeliteOP]] being scheduled
   * @param schedule: [[PartialSchedule]] object with mutable state being updated with state information
   * @param resource: Integer ID of resource (thread) on which DeliteOP is to be scheduled
   */
  protected def scheduleOn(op: DeliteOP, schedule: PartialSchedule, resource: Int) {
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

}
