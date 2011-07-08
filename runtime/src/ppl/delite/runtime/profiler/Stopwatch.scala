package ppl.delite.runtime.profiler

/**
 * Author: Kevin J. Brown
 * Date: Dec 11, 2010
 * Time: 6:33:26 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Stopwatch {

  private var startTime = 0L
  private var theTime = 0L

  def start() {
    startTime = System.currentTimeMillis
  }

  def stop() {
    theTime = System.currentTimeMillis - startTime
  }

  def print() {
    println("Stopwatch time: " + theTime/1000D +"s")
  }
}