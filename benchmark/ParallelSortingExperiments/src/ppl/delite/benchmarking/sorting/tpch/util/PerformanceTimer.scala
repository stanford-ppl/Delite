package ppl.delite.benchmarking.sorting.tpch.util

import collection.mutable.{ArrayBuffer, HashMap}

object PerformanceTimer
{
  val currentTimer = new HashMap[String, Long]
  val times = new HashMap[String, ArrayBuffer[Double]]

  def start(component: String, printMessage: Boolean = true) {
    if (!times.contains(component)) {
      times += component -> new ArrayBuffer[Double]()
    }
    if (printMessage) println("[METRICS]: Timing " + component + " #" + times(component).size + " started")
    currentTimer += component -> System.currentTimeMillis
  }

  def stop(component: String, printMessage: Boolean = true) {
    val x = (System.currentTimeMillis - currentTimer(component)) / 1000D
    times(component) += x
    if (printMessage) println("[METRICS]: Timing " + component + " #" + (times(component).size - 1) + " stopped")
  }

  def totalTime(component: String) {
    val total = times(component).toList.reduceLeft[Double](_+_)
    println("[METRICS]: total time for component " + component + ": " + total)
  }

  def clearAll() {
    for((k,v) <- times) {
      v.clear
    }
  }

  def print(component: String) {
    val timeStr = times.get(component) map { "[METRICS]: Latest time for component " + component + ": " +  _.last.formatted("%.6f") + "s" }
    println(timeStr getOrElse "[METRICS]: No data for component " + component)
  }

}