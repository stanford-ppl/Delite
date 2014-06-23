package ppl.delite.runtime
package profiler

import java.io.{File, PrintWriter}
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import scala.util.parsing.json.JSON
import tools.nsc.io.Path

/**
 * Manages, inputs, and outputs profiling data.
 * 
 * @author Philipp Haller
 */
object Profiler {
  
  var sourceInfo: Map[String, (String, Int, String)] = Map()
  var graph: Option[DeliteTaskGraph] = None
  
  private def getFieldMap(map: Map[Any, Any], field: String): Map[Any,Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any,Any] => map
        case err => error("JSON map not found")
      }
      case None => error("JSON field not found")
    }
  }

  private def getFieldMapOption(map: Map[Any, Any], field: String): Option[Map[Any, Any]] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any,Any] => Some(map)
        case err => None
      }
      case None => None
    }
  }
  
  private def getFieldList(map: Map[Any, Any], field: String): List[Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case list: List[Any] => list
        case err => error("JSON list not found")
      }
      case None => error("JSON field not found")
    }
  }

  private def getFieldString(map: Map[Any, Any], field:String): String = {
    map.get(field) match {
      case Some(field) => field.toString
      case None => error("field not found")
    }
  }

  private def mapFromParsedJSON(json: Any): Map[String, (String, String, Int)] = {
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
  
  def init(taskGraph: DeliteTaskGraph) {
    graph = Some(taskGraph)
  }
  
  private def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
  
  private def deliteOpByIdOrNone(id: String): Option[DeliteOP] =
    graph.get.ops.find(_.id == id)
  
  private def deliteOpType(id: String): String = {
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
  
  private def iterableToJSArray[T](arrName: String, values: Iterable[T], quoted: Boolean = true): String = {
    (for (v <- values) yield (if (quoted) "\"" + v + "\"" else v)).mkString("[", ", ", "]")
  }
  
  private def listOfListsToJSArray[T](arrName: String, values: List[List[T]]): String = {
    (for (v <- values) yield v.mkString("[", ", ", "]")).mkString("[", ", ", "]")
  }
  
  trait TaskInfo {
    val fromTiming: Timing
    val startNanos: Long
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
        val duration =   timing.elapsedMicros
        val startNanos = (timing.startTime - globalStartNanos) / 1000
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
          "<b>Start time:</b> " + ((timing.startTime - globalStartNanos) / 1000) + "us</br>" +
          "<b>Duration:</b> " + timing.elapsedMicros + "us</br>" +
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
    val startNanosJS = iterableToJSArray("start", taskInfos.map(_.startNanos), false)
    val kernelsJS =    iterableToJSArray("kernels", taskInfos.map(_.kernel))
    val locationsJS =  iterableToJSArray("location", taskInfos.map(_.location), false)
    val linesJS =      iterableToJSArray("line_in_source", taskInfos.map(_.line).map("'" + _ + "'"), false)
    val tooltipsJS =   iterableToJSArray("tooltip", taskInfos.map(_.tooltip), false)
    val parallelTasksJS = listOfListsToJSArray("parallelTasks", parallelTaskIndices)

    writer.println("    \"duration\": " + durationJS + ",")
    writer.println("    \"start\": " + startNanosJS + ",")
    writer.println("    \"kernels\": " + kernelsJS + ",")
    writer.println("    \"location\": " + locationsJS)
    //writer.println("    \"line_in_source\": " + linesJS + ",")
    //writer.println("    \"tooltip\": " + tooltipsJS + ",")
    //writer.println("    \"parallelTasks\": " + parallelTasksJS)
  }

  def emitProfileData(file: File, globalStartNanos: Long, jvmUpTimeAtAppStart: Long, timingStats: List[Timing]) {
    val writer = new PrintWriter(file)

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
    
    writer.close()
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
    val directory = Path(Config.profileOutputDirectory).createDirectory()
    val file = directory / "profileData.js"
    emitProfileData(file.jfile, globalStartNanos, jvmUpTimeAtAppStart, timingStats)
  }
  
}
