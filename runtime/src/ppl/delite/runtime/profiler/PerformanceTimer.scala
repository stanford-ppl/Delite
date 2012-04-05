package ppl.delite.runtime.profiler

import collection.mutable
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import ppl.delite.runtime.Config

object PerformanceTimer
{
  val currentTimer = new mutable.HashMap[String, Long]
  // TODO: remove times and only use stats
  val times = new mutable.HashMap[String, mutable.ArrayBuffer[Double]]
  
  var stats: Map[String, List[Timing]] = Map()
  
  // sequence numbers per thread
  val threadSeqNums = new ConcurrentHashMap[String, Integer]
  
  def nextSeqNum(threadName: String): Int = {
    var currSeqNum = threadSeqNums.get(threadName)
    if (currSeqNum == null) {
      currSeqNum = new Integer(0)
    }
    threadSeqNums.put(threadName, new Integer(currSeqNum.intValue() + 1))
    currSeqNum.intValue()
  }
  
  def startChunked(master: String, threadName: String, numChunks: Int, chunk: Int) = synchronized {
    //println("start chunked timing of "+master+", chunk: "+chunk)
    val time = System.nanoTime()
    
    def startMultiTiming(previous: List[Timing]) {
      val timing = new MultiTiming(threadName, time, numChunks, master)
      timing.start(chunk, threadName)
      stats.synchronized {
        stats += master -> (timing :: previous)
      }
    }
    
    // when to start a new timing?
    stats.get(master) match {
      case None =>
        startMultiTiming(List())
      
      case Some(List()) =>
        // should never happen, but repair
        startMultiTiming(List())
      
      case Some((timing: MultiTiming) :: previousTimings) =>
        if (timing(chunk) != null) {
          // timing for chunk already exists
          startMultiTiming(timing :: previousTimings)
        } else {
          timing.start(chunk, threadName)
        }
      
      case Some(timing :: previousTimings) =>
        // most recent timing is not a MultiTiming
        startMultiTiming(timing :: previousTimings)
    }
  }

  def stopChunked(master: String, chunk: Int) = synchronized {
	//println("stop chunked timing of "+master+", chunk: "+chunk)
	
    stats.get(master) match {
      case None =>
        error("cannot stop timing that doesn't exist")
      
      case Some(List()) =>
        error("cannot stop timing that doesn't exist")
      
      case Some((timing: MultiTiming) :: previousTimings) =>
        if (timing(chunk) != null) {
          timing.stop(chunk)
          
          val numToMerge = 1000
          // maybe merge sequence of previous MultiTimings
          if (previousTimings.length >= numToMerge) {
            val preceedingMultiTimings = previousTimings.take(numToMerge).filter(_.isInstanceOf[MultiTiming]).map(_.asInstanceOf[MultiTiming])
            if (preceedingMultiTimings.length == numToMerge) {
              val first = preceedingMultiTimings(numToMerge - 1)
              
              val threadNames = first.timings.map(_.threadName)
              
              val startTimes = first.timings.map(_.startTime)
              
              val timingsPerChunk: List[List[Timing]] = (for (i <- 0 until first.numChunks) yield
                preceedingMultiTimings.map(t => t(i))).toList
              
              val elapsedSums = for (ts <- timingsPerChunk) yield
                ts.foldLeft(0L)((elapsed: Long, t: Timing) => elapsed + t.elapsedMicros)
              
              // create a new MT with threadName, start, numChunks, component from first MT
              val mergedMT = new MultiTiming(first.threadName, first.startTime, first.numChunks, first.component)
              for (i <- 0 until first.numChunks) {
                val tmp = new Timing(threadNames(i), startTimes(i), first.component)
                tmp.endTime = startTimes(i) + (elapsedSums(i) * 1000)
                mergedMT.timings(i) = tmp
              }
              
              // replace previous MTs
              stats += (master -> (timing :: mergedMT :: previousTimings.drop(numToMerge)))
              
              //println("successfully merged multitimings")
            }
          }
        } else {
          error("cannot stop timing that doesn't exist")
        }
    }
  }
  
  // TODO: use Platform instead of System
  def start(component: String, threadName: String, printMessage: Boolean) = synchronized {
    if (!times.contains(component)) {
      times += component -> new mutable.ArrayBuffer[Double]()
      stats.synchronized {
        stats += component -> List[Timing]()
      }
    }
    if (printMessage) println("[METRICS]: Timing " + component + " #" + times(component).size + " started")
    val startTime = System.nanoTime()
    currentTimer += component -> startTime
    
    val previous = stats(component)
    val current = (new Timing(threadName, startTime, component)) :: previous
    stats.synchronized {
      stats += component -> current
    }
  }

  def start(component: String, printMessage: Boolean = true): Unit = {
    //println("thread " + Thread.currentThread().getName() + " executing component " + component)
    start(component, "main", printMessage)
  }
  
  // TODO: use Platform instead of System
  def stop(component: String, printMessage: Boolean = true) = synchronized {
    val endTime = System.nanoTime()
    stats(component) match {
      case List() =>
        // should never happen
        error("cannot stop timing that doesn't exist")
      
      case timing :: previousTimings =>
      	timing.endTime = endTime
    }
    
    val x = (endTime - currentTimer(component)) / 1000D
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

  def print(component: String, globalStart: Long, globalStartNanos: Long) {
    // special case for component == "prof"
    if (component == "prof") {
      Profiler.writeProfile(globalStart, globalStartNanos, stats)
    } else {
      val timeStr = times.get(component) map { time =>
        "[METRICS]: Latest time for component " + component + ": " +  (time.last / 1000000).formatted("%.6f") + "s"
      }
      println(timeStr getOrElse "[METRICS]: No data for component " + component)
    }
  }

  def printProfile(globalStart: Long) {
    def inSecs(v: Long) = (v.toDouble / 1000d).formatted("%.6f")
    
    for (component <- stats.keys) {
      val timingsInSecs = stats(component) map { p =>
        (inSecs(p.startTime - globalStart), inSecs(p.endTime - globalStart))
      }
      
      println("[METRICS]: Timings for component " + component + ": " + timingsInSecs.mkString(" "))
    }
  }
  
  /**
   * dump stats to values provided by config parameters
   */
  def dumpStats() {
    assert(Config.dumpStats)
    dumpStats(Config.dumpStatsComponent)
  }

  def dumpStats(component: String) {
    val directory = Profiler.getOrCreateOutputDirectory()
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