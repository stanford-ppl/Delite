package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.DeliteTaskGraph

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

abstract class StaticScheduler {

  def schedule(graph: DeliteTaskGraph): PartialSchedule

}
