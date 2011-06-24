package ppl.delite.runtime.executor

import ppl.delite.runtime.scheduler.StaticSchedule

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 2:41:22 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * The base class of all executors
 * Defines the public interface for the rest of the Delite Runtime
 */

abstract class Executor {

  def run(schedule: StaticSchedule)

  def init()

  def shutdown()

}
