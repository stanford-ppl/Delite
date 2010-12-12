package ppl.delite.runtime.executor

import ppl.delite.runtime.scheduler.StaticSchedule

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 3:40:38 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * A runtime Executor for a single GPU device
 * This executor spawns a host thread to manage the device
 */
class GPUExecutor extends Executor {

  //TODO: how do we choose the appropriate number of streams for the device?
  val numStreams = 1

  val deviceNum = 0

  val host = new ExecutionThread

  /**
   * The CUDA model requires exactly one host thread per GPU device
   */
  def run(schedule: StaticSchedule) {
    submitAll(schedule)
    val thread = new Thread(host, "GPUHostThread-"+deviceNum) //spawn new machine thread to host GPU device
    thread.setDaemon(true) //to handle shutdown
    thread.start
  }

  /**
   * Puts a static schedule into the appropriate thread queues for execution
   *
   * @param the StaticSchedule to be submitted for execution
   */
  def submitAll(schedule: StaticSchedule) {
    assert(1 == schedule.resources.length)
    val iter = schedule.resources(0).iterator
    while (iter.hasNext) {
      host.queue.put(iter.next)
    }
  }

}
