package ppl.delite.runtime.profiler

import collection.mutable.HashMap
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import scala.collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config

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

  /**
   * dump stats to values provided by config parameters
   */
  def dumpStats() {
    assert(Config.dumpStats)
    dumpStats(Config.dumpStatsComponent)
  }

  def dumpStats(component: String) {
    // check that directory is there or make it
    val directory = new File( Config.statsOutputDirectory)
    if(directory.exists == false)
      directory.mkdirs
    else if(directory.isDirectory == false)
      throw new RuntimeException("statsOutputDirectory doesn't refer to a directory")
    val timesFile = new File(directory.getCanonicalPath + File.separator  + Config.statsOutputFilename)
    if(Config.dumpStatsOverwrite == false && timesFile.exists)
      throw new RuntimeException("stats file " + timesFile + " already exists")
    val fileStream = new PrintWriter(new FileWriter(timesFile))
    dumpStats(component, fileStream)
    fileStream.close
  }

  def dumpStats(component: String, stream: PrintWriter)  {
    times.get(component) map { _.map(_.formatted("%.2f")).mkString("\n") } foreach { stream.print }
  }

}