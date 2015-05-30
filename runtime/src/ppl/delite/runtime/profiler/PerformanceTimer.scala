package ppl.delite.runtime.profiler

import collection.mutable.{ArrayBuffer, Map, Stack}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import ppl.delite.runtime.Config
import java.lang.management.ManagementFactory

object PerformanceTimer {
  var threadCount = 0
  var statsNewFormat = new ArrayBuffer[Map[String, List[Timing]]]()
  val threadIdToWriter = new ArrayBuffer[PrintWriter] // thread id -> thread-specific profile data file
  val threadIdToKernelCallStack = new ArrayBuffer[Stack[Timing]]
  var threadToId: Map[String, Int] = Map()
  var ticTocRegionToTiming: Map[String, Timing] = Map()

  val profileFilePrefix = Config.profileOutputDirectory + "/profile_t_"
  val n = Config.profileOutputDirectory + "/profile_tic_toc_scala.csv"
  val ticTocRegionsWriter = new PrintWriter( new File(n) )
  
  var jvmUpTimeAtAppStart = 0L
  var appStartTimeInMillis = 0L
  var cppStartTime: Long = 0
  var clocksPerSec: Double = 0

  // HACK: This is a temporary solution
  // This is a list of timing data for the component that is tracked using Config.dumpStatsComponent
  var statsForTrackedComponent = new ArrayBuffer[Double]()

  def initializeStats(numThreads: Int) = synchronized {
    threadCount = numThreads

    for (i <- 0 to (numThreads - 1)) {
      val threadName = "ExecutionThread" + i
      threadToId += threadName -> i
      statsNewFormat += Map[String, List[Timing]]()

      val profileFile: File = new File( profileFilePrefix + i + ".csv" )
      threadIdToWriter.append( new PrintWriter(profileFile) )
      threadIdToKernelCallStack.append( new Stack[Timing] )
    }

    statsNewFormat += Map[String, List[Timing]]()
  }

  def recordAppStartTimeStats() = synchronized {
    jvmUpTimeAtAppStart = ManagementFactory.getRuntimeMXBean().getUptime()
    appStartTimeInMillis = System.currentTimeMillis
  }

  def start(component: String, threadName: String, printMessage: Boolean) = {
    val startTime = System.currentTimeMillis
    val threadId = threadToId(threadName)
    val stack = threadIdToKernelCallStack(threadId)
    stack.push(new Timing(threadName, startTime, component))
  }

  def start(component: String, printMessage: Boolean = true): Unit = {
    val startTime = System.currentTimeMillis
	ticTocRegionToTiming += component -> new Timing("main", startTime, component)
  }

  def stop(component: String, threadName: String, printMessage: Boolean) = {
    val endTime = System.currentTimeMillis
    val threadId = threadToId(threadName)
	val stack = threadIdToKernelCallStack(threadId)
	val currKernel = stack.pop()
    if (currKernel.component != component) {
      error( "cannot stop timing that doesn't exist. [TID: " + threadId + ",  Component: " + component + ",  Stack top: " + currKernel.component + "]" )
    }

    currKernel.endTime = endTime
    val writer = threadIdToWriter(threadId)
    writer.println(component + "," + currKernel.startTime + "," + currKernel.elapsedMillis + "," + stack.length)
  }

  def stop(component: String, printMessage: Boolean = true): Unit = {
    val endTime = System.currentTimeMillis
    val t = ticTocRegionToTiming(component)
	t.endTime = endTime
    ticTocRegionsWriter.println(component + "," + t.startTime + "," + t.elapsedMillis + ",0")
	ticTocRegionToTiming -= component
  }

  def stop() {
    for (w <- threadIdToWriter) {
      w.close()
    }

	ticTocRegionsWriter.close()
  }

  // Currently only used by C++ to dump the profile results, need to refactor the code
  def addTiming(component: String, threadId: Int, startTime: Long, endTime: Long, isKernel: Boolean): Unit = {
    // for non-kernel timings (e.g., app) add to the last entry
    val location = if (isKernel) threadId else threadCount
    val threadName = threadToId.find(_._2 == location) match {
      case Some((name,id)) => name
      case None => throw new RuntimeException("cannot find thread name for id " + location)
    }
    
    var stats = statsNewFormat(location)
    val t = new Timing(threadName, startTime, component)
    t.endTime = endTime

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

  def dumpStats(component: String, stream: PrintWriter)  {
    statsNewFormat.foreach(m => {
      if (m.contains(component)) {
        val str = m(component).map(t => t.elapsedMillis).mkString("\n")
        stream.println(str)
      }
    })
  }

  def setCppStartTime(start: Long) {
	cppStartTime = start
    Predef.println("cppStartTime == " + cppStartTime)
  }
}
