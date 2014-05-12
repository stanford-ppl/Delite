package ppl.delite.runtime.profiler

import collection.mutable
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config

object PerformanceTimer
{
  var threadCount = 0
  var statsNewFormat = new ArrayBuffer[Map[String, List[Timing]]]()
  var threadToId: Map[String, Int] = Map()

  def initializeStats(numThreads: Int) = synchronized {
    threadCount = numThreads
    for (i <- List.range(0, numThreads)) {
      val threadName = "ExecutionThread-" + i
      threadToId += threadName -> i
      statsNewFormat += Map[String, List[Timing]]()
    }

    threadToId += "main" -> numThreads
    statsNewFormat += Map[String, List[Timing]]()
  }

  def start(component: String, threadName: String, printMessage: Boolean) = {
    val threadId = threadToId(threadName)
    var stats = statsNewFormat(threadId)

    if (!stats.contains(component)) {
      stats += component -> List[Timing]()
    }

    val startTime = System.nanoTime()
    val previous = stats(component)
    val current = (new Timing(threadName, startTime, component)) :: previous
    stats += component -> current
    statsNewFormat(threadId) = stats
  }

  def start(component: String, printMessage: Boolean = true): Unit = {
    start(component, "main", printMessage)
  }

  def stop(component: String, threadName: String, printMessage: Boolean) = {
    val threadId = threadToId(threadName)
    var stats = statsNewFormat(threadId)
    val endTime = System.nanoTime()
    stats(component) match {
      case List() =>
        error("cannot stop timing that doesn't exist")
      
      case timing :: previousTimings =>
        timing.endTime = endTime
    }
  }

  def stop(component: String, printMessage: Boolean = true): Unit = {
    stop(component, "main", printMessage)
  }

  def clearAll() {
    statsNewFormat.clear()
    initializeStats(threadCount)
  }
  
  def dumpProfile(globalStart: Long, globalStartNanos: Long) { 
    var stats = toList(statsNewFormat)   
    Profiler.writeProfile(globalStart, globalStartNanos, stats)
  }

  def toList(arr: ArrayBuffer[Map[String, List[Timing]]]): List[Timing] = {
    arr.flatMap(m => m.flatMap(kv => kv._2)).toList
  }

  /**
   * dump stats to values provided by config parameters
   */
   /*
  def dumpStats() {
    assert(Config.dumpStats)
    dumpStats(Config.dumpStatsComponent)
  }

  def dumpStats(component: String) {
    val directory = new File(Config.statsOutputDirectory)
    if (directory.exists == false) directory.mkdirs
    else if (directory.isDirectory == false)
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
  */
}