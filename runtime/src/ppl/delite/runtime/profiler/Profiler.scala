package ppl.delite.runtime
package profiler

import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import scala.util.parsing.json.JSON

/**
 * Manages, inputs, and outputs profiling data.
 * 
 * @author Philipp Haller
 */
object Profiler {
  
  var sourceInfo: Map[String, (String, Int, String)] = Map()
  var graph: Option[DeliteTaskGraph] = None
  
  def getFieldMap(map: Map[Any, Any], field: String): Map[Any,Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any,Any] => map
        case err => error("JSON map not found")
      }
      case None => error("JSON field not found")
    }
  }

  def getFieldMapOption(map: Map[Any, Any], field: String): Option[Map[Any, Any]] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any, Any] => Some(map)
        case err => None
      }
      case None => None
    }
  }
  
  def getFieldList(map: Map[Any, Any], field: String): List[Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case list: List[Any] => list
        case err => error("JSON list not found")
      }
      case None => error("JSON field not found")
    }
  }

  def getFieldString(map: Map[Any, Any], field:String): String = {
    map.get(field) match {
      case Some(field) => field.toString
      case None => error("field not found")
    }
  }

  def mapFromParsedJSON(json: Any): Map[String, (String, String, Int)] = {
    val symMap = json match {
      case m: Map[Any, Any] => m
      case err => error("JSON map not found")
    }
    val mappings = getFieldList(symMap, "SymbolMap")
    Map[String, (String, String, Int)]() ++ (for (_mapping <- mappings) yield {
      val mapping = _mapping.asInstanceOf[Map[Any, Any]]
      val symbolName = getFieldString(mapping, "symbol")
      
      // parse source context
      val (fileName, line, opName) = getFieldMapOption(mapping, "sourceContext") match {
      case None =>
        ("<unknown file>", 0, symbolName)
        case Some(sourceContext) =>
        (getFieldString(sourceContext, "fileName"), getFieldString(sourceContext, "line").toInt, getFieldString(sourceContext, "opName"))
      }
      (symbolName, (fileName, opName, line))
    })
  }
  
  def init(info: Map[String, (String, Int, String)], taskGraph: DeliteTaskGraph) {
    sourceInfo = info
    graph = Some(taskGraph)
  }
  
  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
  
  def deliteOpByIdOrNone(id: String): Option[DeliteOP] =
    graph.get.ops.find(_.id == id)
  
  def deliteOpType(id: String): String = {
    val opOpt = deliteOpByIdOrNone(id)
    if (opOpt.isEmpty) "ALL"
    else {
      opOpt.get match {
        case _: OP_Single => "OP_Single"
        case _: OP_External => "OP_External"
        case _: OP_MultiLoop => "OP_MultiLoop"
        case _: OP_Foreach => "OP_Foreach"
        case other =>
          if (other.toString() == "x0") "ARGS"
          else if (other.toString() == "eop") "EOP"
          else //error("OP Type not recognized: " + other)
            ""
      }
    }
  }
  
  def safeSourceInfo(timing: Timing) =
    sourceInfo.get(timing.component) match {
      case None => ("<unknown file>", 0, timing.component)
      case Some(tuple) => tuple
    }
  
  def iterableToJSArray[T](arrName: String, values: Iterable[T], quoted: Boolean = true): String = {
    val arr = (for (v <- values) yield (if (quoted) "\"" + v + "\"" else v)
               ).mkString("[", ", ", "]")
    arr
  }
  
  def listOfListsToJSArray[T](arrName: String, values: List[List[T]]): String = {
    val arr = (for (v <- values) yield v.mkString("[", ", ", "]")).mkString("[", ", ", "]")
    arr
  }
  
  trait TaskInfo {
    val fromTiming: Timing
    //val startNanos: Long
    val start: Long 
    val duration: Long
    val kernel: String
    val location: Int
    val line: String
    val tooltip: String
  }
  
  object TaskInfo {
    def apply(timing: Timing, threadId: Int, globalStartNanos: Long, appendChunkIdToKernelName: Boolean = false): TaskInfo =
      new TaskInfo {
        val fromTiming = timing
        //val duration =   timing.elapsedMicros
        val duration =   timing.elapsedMillis
        //val startNanos = (timing.startTime - globalStartNanos) / 1000
        val start = timing.startTime - globalStartNanos
        val kernel =    if (appendChunkIdToKernelName) timing.component + "_" + threadId else timing.component
        val location =   threadId
        val line = {
          val (fileName, line, opName) = Profiler.sourceInfo.get(timing.component) match {
            case None =>        ("&lt;unknown file&gt;", 0, timing.component)
            case Some(tuple) => tuple
          }
          relativePath(fileName) + ":" + line
        }
        val tooltip = {
          val html =
          //"<b>Start time:</b> " + ((timing.startTime - globalStartNanos) / 1000) + "us</br>" +
          //"<b>Duration:</b> " + timing.elapsedMicros + "us</br>" +
          "<b>OPType:</b> " + deliteOpType(timing.component) + "</br>" +
          "<b>Kernel:</b> " + timing.component + "</br>" +
          "<b>Source:</b> " + line
          "'" + html + "'"
        }
      }
  }
  
  def emitProfileDataArrays(globalStartNanos: Long, stats: List[Timing], writer: PrintWriter) {
    val globalAppStartNanos = globalStartNanos
    val allTimings = stats
    
    val threads = (allTimings.flatMap { timing =>
      timing.threadName :: (timing match {
        case mt: MultiTiming => mt.timings.map(_.threadName).toList
        case other => List()
      })
    }).distinct

    val ThreadName = "^ExecutionThread-(\\d+)$".r
    var threadId: Map[String, Int] = Map()
    for (th <- threads) {
      th match {
        case ThreadName(tid) => threadId += th -> tid.toInt
        case "main" => threadId += th -> (threads.length - 1)
        case _ => println("WARNING: Thread name does not match expected format: " + th)
      }
    }
    
    // output res array (TODO: improve names)
    val resJS = iterableToJSArray("res", (for ((t, i) <- threads.zipWithIndex) yield "T"+i))
    writer.println("    \"res\": " + resJS + ",")
    
    val initTaskInfos =  allTimings map { timing => TaskInfo(timing, threadId(timing.threadName), globalAppStartNanos) }
    
    // expand task infos to include chunk tasks
    val taskInfos = initTaskInfos flatMap { taskInfo =>
      (taskInfo.fromTiming match {
        case mt: MultiTiming =>
          (mt.timings map { chunkTiming =>
            TaskInfo(chunkTiming, threadId(chunkTiming.threadName), globalAppStartNanos, true)
          }).toList
        case other => List(taskInfo)
      })
    }
    
    val parallelTaskIndices = taskInfos map { taskInfo =>
      val component = taskInfo.fromTiming.component
      // generate list of parallel task infos (their indices)
      ((taskInfos zipWithIndex) filter {
        case (info, _) => info.fromTiming.component == component
      }).map(_._2)
    }
    
    val durationJS =   iterableToJSArray("duration", taskInfos.map(_.duration), false)
    //val startNanosJS = iterableToJSArray("start", taskInfos.map(_.startNanos), false)
    val startJS = iterableToJSArray("start", taskInfos.map(_.start), false)
    val kernelsJS =    iterableToJSArray("kernels", taskInfos.map(_.kernel))
    val locationsJS =  iterableToJSArray("location", taskInfos.map(_.location), false)
    val linesJS =      iterableToJSArray("line_in_source", taskInfos.map(_.line).map("'" + _ + "'"), false)
    val tooltipsJS =   iterableToJSArray("tooltip", taskInfos.map(_.tooltip), false)
    val parallelTasksJS = listOfListsToJSArray("parallelTasks", parallelTaskIndices)

    writer.println("    \"duration\": " + durationJS + ",")
    //writer.println("    \"start\": " + startNanosJS + ",")
    writer.println("    \"start\": " + startJS + ",")
    writer.println("    \"kernels\": " + kernelsJS + ",")
    writer.println("    \"location\": " + locationsJS)
    //writer.println("    \"line_in_source\": " + linesJS + ",")
    //writer.println("    \"tooltip\": " + tooltipsJS + ",")
    //writer.println("    \"parallelTasks\": " + parallelTasksJS)
  }

  def emitProfileData(dir: File, fileName: String, globalStartNanos: Long, jvmUpTimeAtAppStart: Long, timingStats: List[Timing]) {
    val dataFile = new File(dir, fileName)
    val fileWriter = new FileWriter(dataFile)
    val writer = new PrintWriter(fileWriter)
    
    def emitProperties(props: List[String]) {
      props.foreach(prop => writer.println("profileDataObj." + prop + " = " + prop + ";"))
    }
    
    writer.println("{\"Profile\":{")
    writer.println("  \"Init\": {")
    writer.println("    \"SystemNanoTimeAtAppStart\": " + globalStartNanos + ",")
    writer.println("    \"JVMUpTimeAtAppStart\": " + jvmUpTimeAtAppStart)
    writer.println("  },")

    writer.println("  \"PerfProfile\": {")
    emitProfileDataArrays(globalStartNanos, timingStats, writer)
    writer.println("  },")
    
    writer.println("")
    MemoryProfiler.dumpProfile(writer)
    writer.println(",")

    SamplerThread.dumpProfile(writer, globalStartNanos)
    writer.println("}}")
    
    writer.flush()
    fileWriter.flush()
    fileWriter.close()
  }

  def dumpProfile(globalStartNanos: Long, jvmUpTimeAtAppStart: Long) {
    val timingStats = PerformanceTimer.getTimingStats()
    writeProfileDataToFile(globalStartNanos, jvmUpTimeAtAppStart, timingStats)
  }
  
  /** Writes profile to JavaScript file (profileData.js).
   *  
   *  Requires system property stats.output.dir to be set.
   */
  def writeProfileDataToFile(globalStartNanos: Long, jvmUpTimeAtAppStart: Long, timingStats: List[Timing]) {
    val directory = getOrCreateOutputDirectory()
    val file = "profileData.js"
	  emitProfileData(directory, file, globalStartNanos, jvmUpTimeAtAppStart, timingStats)
  }
  
  /** Generates HTML profile.
   *  
   *  Requires system properties stats.output.dir, stats.output.filename,
   *  and delite.deg.filename to be set.
   *  
   *  Requires that the following files have been generated: symbols.json, profileData.js
   */
  def main(args: Array[String]) {
    // only generate HTML profile
    // TODO: check that symbols.json and profileData.js have already been generated
  
    // read symbol source info from file
    val symbolsFilename =
      Config.degFilename.substring(0, Config.degFilename.length() - 4) + "-symbols.json"
    val contents = scala.io.Source.fromFile(symbolsFilename).mkString
    
    // maps a symbol (name) to its source context(s): name -> (fileName, opName, line)
    val symbolMap: Map[String, (String, String, Int)] =
      JSON.parseFull(contents) match { // parse JSON into map
        case Some(json) => mapFromParsedJSON(json)
        case None => throw new RuntimeException("Couldn't parse the symbols file")
      }
    
    val htmlFile = new File(getOrCreateOutputDirectory(), Config.statsOutputFilename)
    val fileWriter = new PrintWriter(new FileWriter(htmlFile))
    //Visualizer.writeHtmlProfile(fileWriter, symbolMap)
  }
  
  def getOrCreateOutputDirectory(): File = {
    // check that directory is there or make it
    val directory = new File(Config.profileOutputDirectory)
    if(directory.exists == false)
      directory.mkdirs
    else if(directory.isDirectory == false)
      throw new RuntimeException("profileOutputDirectory doesn't refer to a directory")
    directory
  }
  
}
