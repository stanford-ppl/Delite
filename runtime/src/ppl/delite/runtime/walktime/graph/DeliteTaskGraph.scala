package ppl.delite.runtime.walktime.graph

import java.io.File
import _root_.scala.util.parsing.json.JSON
import collection.mutable.HashMap
import ops.{OP_Single, DeliteOP}

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
    val deg = getFieldMap(degm, "DEG")
    graph.version = getFieldDouble(deg, "version")
    graph.kernelPath = getFieldString(deg, "kernelpath")
    parseOps(getFieldList(deg, "ops"))            
  }

  def parseOps(ops: List[Any])(implicit graph: DeliteTaskGraph) {
    //println("Found: "  + ops)
    for(_op <- ops) {
      val op = _op.asInstanceOf[Map[Any, Any]]
      val opType = getFieldString(op, "type")
      opType match {
        case "SingleTask" => processSingleTask(op)
        case "EOP" => processEOPTask(op)
        case err@_ => unsupportedType(err)
      }
    }

  }

  def getFieldString(map: Map[Any, Any], field:String): String = {
    map.get(field) match {
      case Some(field) => field
      case None => fieldNotFound(field, map)
    }
  }

  def getFieldDouble(map: Map[Any, Any], field: String): Double = {
    map.get(field) match {
      case Some(field) => java.lang.Double.parseDouble(field)
      case None => fieldNotFound(field, map)
    }
  }

  def getFieldList(map: Map[Any, Any], field: String): List[Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case list: List[Any] => list
        case err@_ => listNotFound(err)
      }
      case None => fieldNotFound(field, map)
    }
  }

  def getFieldMap(map: Map[Any, Any], field: String): Map[Any,Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any,Any] => map
        case err@_ => mapNotFound(err)
      }
      case None => fieldNotFound(field, map)
    }
  }

  def getOp(ops: scala.collection.mutable.Map[String, DeliteOP], op: String) = {
    ops.get(op) match {
      case Some(op) => op
      case None => opNotFound(op)
    }
  }

  def processSingleTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    //println("Found Single Task: " + op)
    val newop = new OP_Single
    newop.kernelId = getFieldString(op, "kernelId")
    val types = getFieldMap(op, "return-types")
    newop.scalaResultType = getFieldString(types, "scala")

    //handle inputs
    val inputs = getFieldList(op, "inputs")
    for(i <- inputs) {
      val input = getOp(graph.ops, i)
      newop.addInput(input)
      newop.addDependency(input)
      input.addConsumer(newop)
    }

    //handle anti dependencies
    val antiDeps = getFieldList(op, "antiDeps")
    for(a <- antiDeps) {
      val antiDep = getOp(graph.ops, a)
      newop.addDependency(antiDep)
      antiDep.addConsumer(newop)
    }

    //handle control dependencies
    val controlDeps = getFieldList(op, "controlDeps")
    for(c <- controlDeps) {
      val controlDep = getOp(graph.ops, c)
      newop.addDependency(controlDep)
      controlDep.addConsumer(newop)
    }

    //add new op to graph list of ops
    graph.ops += newop.kernelId -> newop

    //last op will be result op
    graph._result = newop

  }

  def processEOPTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    //println("Found EOP Task: " + op)
  }

  def unsupportedType(err:String) = throw new RuntimeException("Unsupported Op Type found: " + err)
  def fieldNotFound(field: String, obj: Any) = throw new RuntimeException("Expecting field [" + field + "], found: " + obj )
  def mapNotFound(err:Any) = throw new RuntimeException("Expecting a Map object, found: " + err)
  def listNotFound(err:Any) = throw new RuntimeException("Expecting a List object, found: " + err)
  def opNotFound(op:String) = throw new RuntimeException("Couldn't find following op: " + op)
}


class DeliteTaskGraph {


  val ops = new HashMap[String, DeliteOP]
  var _result:DeliteOP = _

  var version = 0.0
  var kernelPath = ""


  
  def result : DeliteOP = _result

}