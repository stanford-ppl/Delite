package ppl.delite.runtime.executor.gpu

import ppl.delite.runtime.codegen.DeliteExecutable
import ppl.delite.runtime.scheduler.StaticSchedule

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 5:32:42 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

@deprecated
class StreamPool(numStreams: Int) {

  val pool = new Array[ExecutionStream](numStreams)

  //TODO: CUDA model requires one host thread per device => we should fork a thread for each instance
  def init {

  }

  def submitOne(id: Int, work: DeliteExecutable) {
    pool(id).put(work)
  }

  def submitAll(schedule: StaticSchedule) {
    assert(pool.length == schedule.resources.length)
    for (i <- 0 until pool.length) {
      for (j <- 0 until schedule.resources(i).size) {
        pool(i).put(schedule.resources(i).poll)
      }
    }
  }

}
