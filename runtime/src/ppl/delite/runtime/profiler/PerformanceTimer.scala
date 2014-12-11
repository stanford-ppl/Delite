package ppl.delite.runtime.profiler

import collection.mutable.{ArrayBuffer, Map}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import ppl.delite.runtime.Config
import java.lang.management.ManagementFactory

object PerformanceTimer {
  var threadCount = 0
  var statsNewFormat = new ArrayBuffer[Map[String, List[Timing]]]()
  var threadToId: Map[String, Int] = Map()
  
  var jvmUpTimeAtAppStart = 0L
  var appStartTimeInMillis = 0L

  // HACK: This is a temporary solution
  // This is a list of timing data for the component that is tracked using Config.dumpStatsComponent
  var statsForTrackedComponent = new ArrayBuffer[Double]()

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

  def recordAppStartTimeStats() = synchronized {
    jvmUpTimeAtAppStart = ManagementFactory.getRuntimeMXBean().getUptime()
    appStartTimeInMillis = System.currentTimeMillis
  }

  def start(component: String, threadName: String, printMessage: Boolean) = {
    val threadId = threadToId(threadName)
    var stats = statsNewFormat(threadId)

    if (!stats.contains(component)) {
      stats += component -> List[Timing]()
    }

    val startTime = System.currentTimeMillis
    val previous = stats(component)
    val current = (new Timing(threadName, startTime, component)) :: previous
    stats += component -> current
  }

  def start(component: String, printMessage: Boolean = true): Unit = {
    start(component, "main", printMessage)
  }

  def stop(component: String, threadName: String, printMessage: Boolean) = {
    val threadId = threadToId(threadName)
    var stats = statsNewFormat(threadId)
    val endTime = System.currentTimeMillis 
    stats(component) match {
      case List() =>
        error("cannot stop timing that doesn't exist")
      
      case timing :: previousTimings =>
        timing.endTime = endTime
        if (component == Config.dumpStatsComponent) {
<<<<<<< HEAD
          statsForTrackedComponent += (timing.endTime - timing.startTime)
=======
          statsForTrackedComponent += (timing.endTime - timing.startTime) / 1e3
>>>>>>> develop
        }
    }
  }

  def stop(component: String, printMessage: Boolean = true): Unit = {
    stop(component, "main", printMessage)
  }

  // Currently only used by C++ to dump the profile results, need to refactor the code
<<<<<<< HEAD
  def addTiming(component: String, threadId: Int, startTime: Long, endTime: Long): Unit = {
    Predef.println((startTime < endTime) + " startTime: " + startTime + "  endTime: " + endTime)
    val threadName = threadToId.find(_._2 == threadId) match { 
=======
  def addTiming(component: String, threadId: Int, startTime: Long, endTime: Long, isKernel: Boolean): Unit = {
    // for non-kernel timings (e.g., app) add to the last entry
    val location = if (isKernel) threadId else threadCount
    val threadName = threadToId.find(_._2 == location) match {
>>>>>>> develop
      case Some((name,id)) => name
      case None => throw new RuntimeException("cannot find thread name for id " + location)
    }
<<<<<<< HEAD
    var stats = statsNewFormat(threadId)
    val t = new Timing(threadName, startTime/1000L, component)
    t.endTime = endTime/1000L
=======
    var stats = statsNewFormat(location)
    val t = new Timing(threadName, startTime, component)
    t.endTime = endTime
>>>>>>> develop
    if (!stats.contains(component)) {
      stats += component -> List[Timing]()
    }
    stats += component -> (t :: stats(component))
    
    if (component == Config.dumpStatsComponent) {
      statsForTrackedComponent += (endTime - startTime) / 1e3
    }
  }

  def clearAll() {
    statsNewFormat.clear()
    initializeStats(threadCount)
  }

  def getTimingStats(): List[Timing] = {
    toList(statsNewFormat)
  }

  def toList(arr: ArrayBuffer[Map[String, List[Timing]]]): List[Timing] = {
    arr.flatMap(m => m.flatMap(kv => kv._2)).toList
  }

  def printStatsForNonKernelComps() {
    val nonKernelCompsToTimings = statsNewFormat(threadCount) // the last entry in the stats array does not correspond 
                    // to an actual execution thread. Rather, it just stores data for 'all' and tic-toc regions
    nonKernelCompsToTimings.foreach(kv => {
      kv._2.foreach(timing => {
        //val str = "[METRICS]: Time for component " + kv._1 + ": " +  (timing.elapsedMicros.toFloat / 1000000).formatted("%.6f") + "s"
        val str = "[METRICS]: Time for component " + kv._1 + ": " +  (timing.elapsedMillis.toFloat / 1000).formatted("%.3f") + "s"
        println(str)
      })
    })
  }

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
    //dumpStats(fileStream)
    dumpStats(component, fileStream)
    fileStream.close
  }

/*
  def dumpStats(stream: PrintWriter)  {
    val s = statsForTrackedComponent.map(t => t.formatted("%.2f")).mkString("\n")
    stream.print(s)
*/

  def dumpStats(component: String, stream: PrintWriter)  {
    statsNewFormat.foreach(m => {
      if (m.contains(component)) {
        //val str = m(component).map(t => t.elapsedMicros).mkString("\n")
        val str = m(component).map(t => t.elapsedMillis).mkString("\n")
        stream.println(str)
      }
    })
  }
}
