package ppl.delite.runtime.graph

import java.io.File
import _root_.scala.util.parsing.json.JSON
import ops._
import collection.mutable.HashMap
import targets._

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
    graph._version = getFieldDouble(deg, "version")
    graph._kernelPath = getFieldString(deg, "kernelpath")
    parseOps(getFieldList(deg, "ops"))
  }

  def parseOps(ops: List[Any])(implicit graph: DeliteTaskGraph) {
    //println("Found: "  + ops)
    for(_op <- ops) {
      val op = _op.asInstanceOf[Map[Any, Any]]
      val opType = getFieldString(op, "type")
      opType match {
        case "SingleTask" => processCommon(op, "OP_Single")
        case "MapReduce" => processCommon(op, "OP_MapReduce")
        case "Arguments" => processArgumentsTask(op)
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

  def processCommon(op: Map[Any, Any], opType: String)(implicit graph: DeliteTaskGraph) {
    val id = getFieldString(op, "kernelId")

    val targets = getFieldList(op, "supportedTargets")
    val types = getFieldMap(op, "return-types")
    var resultMap = Map[Targets.Value,String]()
    for (target <- Targets.values) {
      if (targets.contains(target.toString)) {
        resultMap += target -> getFieldString(types, target.toString)
      }
    }

    val newop = opType match {
      case "OP_Single" => new OP_Single("kernel_"+id, resultMap)
      case "OP_MapReduce" => new OP_MapReduce("kernel_"+id, resultMap)
      case other => error("OP Type not recognized: " + other)
    }

    //handle inputs
    val inputs = getFieldList(op, "inputs")
    for(i <- inputs.reverse) {
      val input = getOp(graph._ops, i)
      newop.addInput(input)
      newop.addDependency(input)
      input.addConsumer(newop)
    }

    //handle anti dependencies
    val antiDeps = getFieldList(op, "antiDeps")
    for(a <- antiDeps) {
      val antiDep = getOp(graph._ops, a)
      newop.addDependency(antiDep)
      antiDep.addConsumer(newop)
    }

    //handle control dependencies
    val controlDeps = getFieldList(op, "controlDeps")
    for(c <- controlDeps) {
      val controlDep = getOp(graph._ops, c)
      newop.addDependency(controlDep)
      controlDep.addConsumer(newop)
    }

    //process target metadata
    if (resultMap.contains(Targets.Cuda)) processCudaMetadata(op, newop)

    //add new op to graph list of ops
    graph._ops += id -> newop

    //last op will be result op
    graph._result = newop

  }

  /**
   * Add the Arguments op to the task graph
   * This op feeds all application ops that consume command line arguments
   * By definition it has no dependencies
   */
  def processArgumentsTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val kernelId = getFieldString(op, "kernelId")
    graph._ops += kernelId -> Arguments
    graph._result = Arguments
  }

  /**
   * Add the EOP op to the task graph
   * This op follows the application result
   */
  def processEOPTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val result = graph._result
    EOP.addDependency(result) //EOP depends on "result" of application
    result.addConsumer(EOP)
    graph._result = EOP //set EOP as "result" for scheduler
  }

  /**
   * Extract the required Cuda metadata from the DEG
   */
  def processCudaMetadata(op: Map[Any, Any], newop: DeliteOP)(implicit graph: DeliteTaskGraph) {
    val metadataAll = getFieldMap(op, "metadata")
    val metadataMap = getFieldMap(metadataAll, "cuda")
    val cudaMetadata = newop.cudaMetadata

    def fill(field: String) {
      val list = getFieldList(metadataMap, field)
      val data = cudaMetadata(field)
      data.func = list.head
      for (sym <- list.tail.head.asInstanceOf[List[String]].reverse) data.inputs ::= getOp(graph._ops, sym)
    }

    fill("gpuBlockSizeX") //threads/block - x
    fill("gpuBlockSizeY") //threads/block - y
    fill("gpuBlockSizeZ") //threads/block - z
    fill("gpuDimSizeX") //blocks in grid - x
    fill("gpuDimSizeY") //blocks in grid - y

    for (input <- getFieldList(metadataMap, "gpuInputs").reverse) { //input list
      val value = (input.asInstanceOf[Map[String,Any]].values.head).asInstanceOf[List[Any]]
      val data = cudaMetadata.newInput
      data.resultType = value.head
      data.func = value.tail.head
    }

    val tempSyms = new HashMap[String,DeliteOP]
    for (temp <- getFieldList(metadataMap, "gpuTemps").reverse) {
      val tempOp = new OP_Single(null, null)
      tempSyms += temp.toString -> tempOp
      cudaMetadata.tempOps ::= tempOp
    }

    def getOpLike(sym: String) = graph._ops.getOrElse(sym, tempSyms(sym))

    for (temp <- getFieldList(metadataMap, "gpuTemps").reverse) { //temporaries list
      val value = (temp.asInstanceOf[Map[String,Any]].values.head).asInstanceOf[List[Any]]
      val data = cudaMetadata.newTemp
      data.resultType = value.head
      data.func = value.tail.head
      for (sym <- value.tail.tail.head.asInstanceOf[List[String]].reverse) {
        data.inputs ::= getOpLike(sym)
      }
    }

    //output allocation
    val outList = getFieldMap(metadataMap, "gpuOutput").values.head.asInstanceOf[List[Any]]
    cudaMetadata.outputAlloc.resultType = outList.head
    cudaMetadata.outputAlloc.func = outList.tail.head
    for (sym <- outList.tail.tail.head.asInstanceOf[List[String]].reverse) {
      cudaMetadata.outputAlloc.inputs ::= getOpLike(sym)
    }
    //output copy
    cudaMetadata.outputSet.func = outList.tail.tail.tail.head

  }

  def unsupportedType(err:String) = throw new RuntimeException("Unsupported Op Type found: " + err)
  def fieldNotFound(field: String, obj: Any) = throw new RuntimeException("Expecting field [" + field + "], found: " + obj )
  def mapNotFound(err:Any) = throw new RuntimeException("Expecting a Map object, found: " + err)
  def listNotFound(err:Any) = throw new RuntimeException("Expecting a List object, found: " + err)
  def opNotFound(op:String) = throw new RuntimeException("Couldn't find following op: " + op)
}


class DeliteTaskGraph {


  val _ops = new HashMap[String, DeliteOP]
  var _result: DeliteOP = _

  var _version = 0.0
  var _kernelPath = ""



  def result : DeliteOP = _result
  def version: Double = _version
  def kernelPath: String = _kernelPath
  def ids: Iterable[String] = _ops.keys
  def ops: Iterable[DeliteOP] = _ops.values

}