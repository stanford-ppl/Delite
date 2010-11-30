package ppl.delite.runtime.walktime.graph

import collection.mutable.ArrayBuffer
import ops.DeliteOP
import java.io.{File}
import _root_.scala.util.parsing.json.{JSONObject, JSON}

object DeliteTaskGraph {

  //implicit conversion to strings
  implicit def anyToString(obj: Any): String = obj.toString

  def apply(degFile: File) = {
    val contents = scala.io.Source.fromFile(degFile).mkString
    JSON.parseFull(contents) match {
      case Some(json) => buildFromParsedJSON(json)
      case None => throw new RuntimeException("Couldn't parse the DEG file")
    }

  }

  def buildFromParsedJSON(json: Any) = {
    implicit val graph = new DeliteTaskGraph
    json match {
      case degm: Map[Any,Any] => parseDEGMap(degm)
      case err@_ => mapNotFound(err)
    }
    graph    
  }

  def parseDEGMap(degm: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val deg = degm.get("DEG") match {
      case Some(deg) => deg match {  
        case deg: Map[Any, Any] => {
          println(deg)
          graph.version = deg.get("version") match {
            case Some(version) => java.lang.Double.parseDouble(version)
            case None => fieldNotFound("version", deg)
          }
          graph.kernelPath = deg.get("kernelpath") match {
            case Some(path) => path
            case None => fieldNotFound("kernelpath", deg)
          }
          deg.get("ops") match {
            case Some(ops) => ops match {
              case ops: List[Any] => parseOps(ops)
              case err@_ => listNotFound(ops)
            }
            case None => fieldNotFound("ops", deg)
          }
        }
        case err@_ => mapNotFound(err)
      }
      case None => throw new RuntimeException("Expecting Map with DEG element, found: " + degm)
    }
  }

  def parseOps(ops: Any)(implicit graph: DeliteTaskGraph) {
    println("Found: "  + ops)    
  }

  def fieldNotFound(field: String, obj: Any) = throw new RuntimeException("Expecting field [" + field + "], found: " + obj )
  def mapNotFound(err:Any) = throw new RuntimeException("Expecting a Map object, found: " + err)
  def listNotFound(err:Any) = throw new RuntimeException("Expecting a List object, found: " + err)
}
class DeliteTaskGraph {


  val ops = new ArrayBuffer[DeliteOP]
  var _result:DeliteOP = _

  var version = 0.0
  var kernelPath = ""

  def addOP(kernelId: String, opType: DeliteOP)(deps: DeliteOP*) = {
    
  }

  
  def result : DeliteOP = _result

}