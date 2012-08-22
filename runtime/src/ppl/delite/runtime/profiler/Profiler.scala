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
    "var " + arrName + " = " + arr + ";"
  }
  
  def listOfListsToJSArray[T](arrName: String, values: List[List[T]]): String = {
    val arr = (for (v <- values) yield v.mkString("[", ", ", "]")).mkString("[", ", ", "]")
    "var " + arrName + " = " + arr + ";"
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
    def apply(timing: Timing, threadId: Int, globalStartNanos: Long): TaskInfo =
      new TaskInfo {
        val fromTiming = timing
        val duration =   timing.elapsedMicros
        val startNanos = (timing.startTime - globalStartNanos) / 1000
        val kernel =     safeSourceInfo(timing)._3
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
  
  def emitProfileDataArrays(globalStartNanos: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
/*
var duration = [280, 800, 400, 500, 1500, 600];
var start = [1000, 10, 800, 820, 2, 200];
var kernels = ["x174", "x277", "x107", "x135", "x108", "x99"];
var location = [0, 1, 2, 1, 3, 0];
var line_in_source = ['&lt;unknown file&gt;:0','HelloWorld4.scala:8','HelloWorld4.scala:9','HelloWorld4.scala:12','HelloWorld4.scala:14','&lt;unknown file&gt;:0'];
var tooltip = ['<b>Start time: </b>1000us </br><b>Duration:</b> 280us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>10us </br><b>Duration:</b> 800us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>800us </br><b>Duration:</b> 400us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>820us </br><b>Duration:</b> 500us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>2us </br><b>Duration:</b> 1500us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>200us </br><b>Duration:</b> 600us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9'];

var res = ["T0", "T1", "T2", "T3", "T4", "T5"];

var parallelTasks = [[1, 4, 6], [2, 3, 5]];
*/
    
    val allInitTimings = (for (id <- stats.keys; timing <- stats(id)) yield timing).toList

    // for skipping input
    val globalAppStartNanos = stats.get("app") match {
      case Some(appStats) =>
      	//println("found stats for app")
        val appStart = appStats(0).startTime
        //println("app start nanos: "+appStart)
      	appStart
      case None =>
        globalStartNanos        
    }
    
    // only include timings started after globalAppStartNanos
    val allTimings = allInitTimings filter { _.startTime >= globalAppStartNanos }
    
    val threads = (allTimings.flatMap { timing =>
      timing.threadName :: (timing match {
        case mt: MultiTiming => mt.timings.map(_.threadName).toList
        case other => List()
      })
    }).distinct
    val threadId: Map[String, Int] = Map() ++ (for (elem <- threads zipWithIndex) yield elem)
    
    // output res array (TODO: improve names)
    val resJS = iterableToJSArray("res", (for ((t, i) <- threads.zipWithIndex) yield "T"+i))
    writer.println(resJS)
    
    val initTaskInfos =  allTimings map { timing => TaskInfo(timing, threadId(timing.threadName), globalAppStartNanos) }
    
    // expand task infos to include chunk tasks
    val taskInfos = initTaskInfos flatMap { taskInfo =>
      taskInfo :: (taskInfo.fromTiming match {
        case mt: MultiTiming =>
          (mt.timings map { chunkTiming =>
            TaskInfo(chunkTiming, threadId(chunkTiming.threadName), globalAppStartNanos)
          }).toList
        case other => List()
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
    
    writer.println(durationJS)
    writer.println(startNanosJS)
    writer.println(kernelsJS)
    writer.println(locationsJS)
    writer.println(linesJS)
    writer.println(tooltipsJS)
    writer.println(parallelTasksJS)
  }

  def emitProfileData(dir: File, fileName: String, globalStartNanos: Long, stats: Map[String, List[Timing]]) {
    val dataFile = new File(dir, fileName)
    val fileWriter = new FileWriter(dataFile)
    val writer = new PrintWriter(fileWriter)
    
    def emitProperties(props: List[String]) {
      props.foreach(prop => writer.println("profileDataObj." + prop + " = " + prop + ";"))
	}
    
    writer.println("function profileData() {")
    emitProfileDataArrays(globalStartNanos, stats, writer)
    writer.println("var profileDataObj = new Object();")
    emitProperties(List("res", "duration", "start", "kernels", "location", "line_in_source", "tooltip"))
    writer.println("return profileDataObj; }")
    
    writer.flush()
    fileWriter.flush()
    fileWriter.close()
  }
  
  /** Writes profile to JavaScript file (profileData.js).
   *  
   *  Requires system property stats.output.dir to be set.
   */
  def writeProfile(globalStart: Long, globalStartNanos: Long, stats: Map[String, List[Timing]]) {
    val directory = getOrCreateOutputDirectory()
    // emit JS file containing the profile data
	  emitProfileData(directory, "profileData.js", globalStartNanos, stats)
  }
  
  def writeProfile(globalStart: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
    for (id <- stats.keys) {
      val timings = stats(id).flatMap(p => {
        val postfix = if (p.isInstanceOf[MultiTiming]) {
          "MultiLoop: " + p.asInstanceOf[MultiTiming].timings.mkString(" ")
        } else
          ""
        
        val source = sourceInfo.get(id) match {
          case None => "<unknown file>"
          case Some((fileName, line, opName)) => fileName + ":" + line
        }
        
        val microsElapsed = (p.endTime - p.startTime) / 1000        
        List(p.threadName, microsElapsed + "us", postfix, source)
      })
      writer.println(id + " " + timings.mkString(" "))
    }
    writer.flush()
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
    Visualizer.writeHtmlProfile(fileWriter, symbolMap)
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
