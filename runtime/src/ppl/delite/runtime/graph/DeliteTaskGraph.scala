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
    for(_op <- ops) {
      val op = _op.asInstanceOf[Map[Any, Any]]
      val opType = getFieldString(op, "type")
      opType match {
        case "SingleTask" => processCommon(op, "OP_Single")
        case "MultiLoop" => processCommon(op, "OP_MultiLoop")
        case "MapReduce" => processCommon(op, "OP_MapReduce")
        case "Map" => processCommon(op, "OP_Map")
        case "Reduce" => processCommon(op, "OP_Reduce")
        case "ZipWith" => processCommon(op, "OP_Zip")
        case "Foreach" => processCommon(op, "OP_Foreach")
        case "Conditional" => processIfThenElseTask(op)
        case "WhileLoop" => processWhileTask(op)
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

  def getFieldBoolean(map: Map[Any, Any], field: String): Boolean = {
    map.get(field) match {
      case Some(field) => java.lang.Boolean.parseBoolean(field)
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

    val lookupTarget = Targets.values.map(x=>(x.toString->x)).toMap

    val targets = getFieldList(op, "supportedTargets")
    val types = getFieldMap(op, "return-types")
    var resultMap = Map[Targets.Value,String]()
    for (target <- Targets.values) {
      if (targets.contains(target.toString)) {
        resultMap += target -> getFieldString(types, target.toString)
      }
    }

    val newop = opType match {
      case "OP_Single" => new OP_Single(id, "kernel_"+id, resultMap)
      case "OP_MultiLoop" => new OP_MultiLoop(id, "kernel_"+id, resultMap, getFieldBoolean(op, "needsCombine"))
      case "OP_MapReduce" => new OP_MapReduce(id, "kernel_"+id, resultMap)
      case "OP_Map" => new OP_Map(id, "kernel_"+id, resultMap)
      case "OP_Reduce" => new OP_Reduce(id, "kernel_"+id, resultMap)
      case "OP_Zip" => new OP_Zip(id, "kernel_"+id, resultMap)
      case "OP_Foreach" => new OP_Foreach(id, "kernel_"+id, resultMap)
      case other => system.error("OP Type not recognized: " + other)
    }

    //TR decoupling input->op from input->string identifier

    //handle outputs
    val outputs = getFieldList(op, "outputs")
    if (op.contains("output-types")) {
      val outputTypes = getFieldMap(op, "output-types")
      for(i <- outputs.reverse) {
        val tp = getFieldMap(outputTypes,i).map(p=>(lookupTarget(p._1), p._2.toString))
        newop.addOutput(i, tp)
      }
      println("putput "+newop+" "+outputTypes)
    } else {
      assert(outputs.size == 1)
      newop.addOutput(outputs.head, resultMap)
    }

    //handle inputs
    val inputs = getFieldList(op, "inputs")
    for(i <- inputs.reverse) {
      val input = graph.getOp(i)
      newop.addInput(input, i)
      newop.addDependency(input)
      input.addConsumer(newop)
    }

    //handle mutable inputs
    val mutableInputs = getFieldList(op, "mutableInputs")
    for (m <- mutableInputs) {
      val mutable = graph.getOp(m)//getOp(graph._ops, m)
      newop.addMutableInput(mutable)
    }

    //handle anti dependencies
    val antiDeps = getFieldList(op, "antiDeps")
    for(a <- antiDeps) {
      val antiDep = graph.getOp(a)
      newop.addDependency(antiDep)
      antiDep.addConsumer(newop)
    }

    //handle control dependencies
    val controlDeps = getFieldList(op, "controlDeps")
    for(c <- controlDeps) {
      val controlDep = graph.getOp(c)
      newop.addDependency(controlDep)
      controlDep.addConsumer(newop)
    }

    //add new op to graph list of ops
    graph.registerOp(newop)
    //if (graph._ops.contains(id)) error("Op " + id + " is declared multiple times in DEG")
    //graph._ops += id -> newop

    //process target metadata
    if (resultMap.contains(Targets.Cuda)) processCudaMetadata(op, newop)

    //last op will be result op
    graph._result = newop

  }

  def processIfThenElseTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")
    //get predicate, then, and else kernels
    val predOp = getOp(graph._ops, getFieldString(op, "conditionKernelId"))
    val thenIds = getFieldList(op, "thenKernelIds")
    val elseIds = getFieldList(op, "elseKernelIds")
    val beginOp = new OP_BeginCondition(id+"b", predOp)
    val beginElseOp = new OP_BeginElse(id+"e")
    val endOp = new OP_EndCondition(id)

    var thenOps: List[DeliteOP] = Nil
    for (thenId <- thenIds) thenOps ::= getOp(graph._ops, thenId)
    val thenOpsBegin = thenOps.filterNot(_.isInstanceOf[OP_Control]) ++ thenOps.filter(_.isInstanceOf[OP_Control]).map(o => getOp(graph._ops, o.id+"b"))
    var elseOps: List[DeliteOP] = Nil
    for (elseId <- elseIds) elseOps ::= getOp(graph._ops, elseId)
    val elseOpsBegin = elseOps.filterNot(_.isInstanceOf[OP_Control]) ++ elseOps.filter(_.isInstanceOf[OP_Control]).map(o => getOp(graph._ops, o.id+"b"))

    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var ifDeps: List[DeliteOP] = Nil
    for (depId <- depIds) ifDeps ::= getOp(graph._ops, depId)
    //list of all dependencies of the if block, minus any dependencies within the block
    val ignore = (thenOpsBegin ++ elseOpsBegin).filter(_.isInstanceOf[OP_Control]).map(o => o.asInstanceOf[OP_Control].predicate)
    ifDeps = (ifDeps ++ thenOpsBegin.flatMap(_.getDependencies) ++ elseOpsBegin.flatMap(_.getDependencies)) filterNot { (thenOps ++ elseOps ++ ignore) contains }

    //beginning depends on all exterior dependencies
    for (dep <- ifDeps) {
      beginOp.addDependency(dep)
      dep.addConsumer(beginOp)
    }

    //beginning depends on predicate resolution
    beginOp.addDependency(predOp)
    predOp.addConsumer(beginOp)

    //all thenOps depend on beginning, and begin-else depends on them
    for (thenOp <- thenOpsBegin) {
      thenOp.addDependency(beginOp)
      beginOp.addConsumer(thenOp)
    }
    for (thenOp <- thenOps) {
      thenOp.addConsumer(beginElseOp)
      beginElseOp.addDependency(thenOp)
    }

    //all elseOps depend on begin-else, ending depends on them
    for (elseOp <- elseOpsBegin) {
      elseOp.addDependency(beginElseOp)
      beginElseOp.addConsumer(elseOp)
    }
    for (elseOp <- elseOps) {
      elseOp.addConsumer(endOp)
      endOp.addDependency(elseOp)
    }

    //add a direct link between begin/end else because else block can be empty
    endOp.addDependency(beginElseOp)
    beginElseOp.addConsumer(endOp)

    //add to graph
    //graph.registerOp(beginOp)
    //graph.registerOp(beginElseOp)
    //graph.registerOp(endOp)
    graph._ops += id+"b" -> beginOp
    graph._ops += id+"e" -> beginElseOp
    graph._ops += id -> endOp //endOp will be found by future ops when searching by graph id
    // TODO: fix up outputs/outputSlotTypes and use graph.registerOp

    graph._result = endOp
  }

  def processWhileTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")
    //get predicate and body kernels
    val predIds = getFieldList(op, "condIds")
    val bodyIds = getFieldList(op, "bodyIds")

    var predOps: List[DeliteOP] = Nil
    var bodyOps: List[DeliteOP] = Nil
    var contOps: List[DeliteOP] = Nil

    //copy the predicate in order to run it after the body as well
    for (predId <- predIds) {
      val predOp = getOp(graph._ops, predId)
      assert(predOp.isInstanceOf[OP_Single]) //TODO: should this be relaxed?
      val contOp = predOp.asInstanceOf[OP_Single].duplicate(predOp.id+"p")
      contOp.consumerList = Nil
      graph._ops += contOp.id -> contOp

      predOps ::= predOp
      contOps ::= contOp
    }
    for (bodyId <- bodyIds) bodyOps ::= getOp(graph._ops, bodyId)
    val bodyOpsBegin = bodyOps.filterNot(_.isInstanceOf[OP_Control]) ++ bodyOps.filter(_.isInstanceOf[OP_Control]).map(o => getOp(graph._ops, o.id+"b"))

    //fix up the internal dependencies of the copied predicate ops
    for (contOp <- contOps) {
      for (dep <- contOp.getDependencies) {
        val idx = predOps findIndexOf { _ == dep }
        if (idx != -1) {
          val newOp = contOps(idx)
          contOp.replaceDependency(dep, newOp)
          newOp.addConsumer(contOp)
        }
      }
      for ((input,name) <- contOp.getInputs) {
        val idx = predOps findIndexOf { _ == input }
        if (idx != -1) contOp.replaceInput(input, contOps(idx), name) // TODO: check
      }
    }

    val beginOp = new OP_BeginWhile(id+"b", predOps.head)
    val endOp = new OP_EndWhile(id, contOps.head)

    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var whileDeps: List[DeliteOP] = Nil
    for (depId <- depIds) whileDeps ::= getOp(graph._ops, depId)
    //list of all dependencies of the while block, minus any dependencies within the block
    val ignore = bodyOpsBegin.filter(_.isInstanceOf[OP_Control]).map(o => o.asInstanceOf[OP_Control].predicate)
    whileDeps = (whileDeps ++ bodyOpsBegin.flatMap(_.getDependencies)) filterNot { (bodyOps ++ ignore) contains }

    //beginning depends on all exterior dependencies
    for (dep <- whileDeps) {
      beginOp.addDependency(dep)
      dep.addConsumer(beginOp)
    }

    //beginning depends on initial predicate resolution
    beginOp.addDependency(predOps.head)
    predOps.head.addConsumer(beginOp)

    //all bodyOps depend on beginning, predOps depend on them
    for (bodyOp <- bodyOpsBegin) {
      bodyOp.addDependency(beginOp)
      beginOp.addConsumer(bodyOp)
    }
    for (bodyOp <- bodyOps) {
      for (contOp <- contOps) {
        contOp.addDependency(bodyOp)
        bodyOp.addConsumer(contOp)
      }
    }

    //ending depends on final contOp
    contOps.head.addConsumer(endOp)
    endOp.addDependency(contOps.head)

    //add to graph
    graph._ops += id+"b" -> beginOp
    graph._ops += id -> endOp //endOp will be found by future ops when searching by graph id
    // TODO: fix up outputs/outputSlotTypes and use graph.registerOp

    graph._result = endOp
  }

  /**
   * Add the Arguments op to the task graph
   * This op feeds all application ops that consume command line arguments
   * By definition it has no dependencies
   */
  def processArgumentsTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val kernelId = getFieldString(op, "kernelId")
    Arguments.id = kernelId
    Arguments.addOutput(kernelId, Map(Targets.Scala -> Arguments.outputType))
    graph.registerOp(Arguments)
    //graph._ops += kernelId -> Arguments
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

    //check library call
    cudaMetadata.libCall = metadataMap.get("gpuLibCall") match {
      case Some(x) => x
      case None => null
    }

    for (input <- getFieldList(metadataMap, "gpuInputs").reverse) { //input list
      val value = (input.asInstanceOf[Map[String,Any]].values.head).asInstanceOf[List[Any]]
      val data = cudaMetadata.newInput
      data.resultType = value.head
      data.func = value.tail.head
      data.funcReturn = value.tail.tail.head
    }

    val tempSyms = new HashMap[String,DeliteOP]
    for (temp <- getFieldList(metadataMap, "gpuTemps").reverse) {
      val key = (temp.asInstanceOf[Map[String,Any]].keys.head)
      val tempOp = new OP_Single(key, null, null)
      tempSyms += key -> tempOp
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
    metadataMap.get("gpuOutput") match {
      case None => //do nothing
      case Some(field) => field match {
        case out: Map[Any,Any] => {
          val outList = out.values.head.asInstanceOf[List[Any]]
          cudaMetadata.output.resultType = outList.head
          cudaMetadata.output.func = outList.tail.head
          for (sym <- outList.tail.tail.head.asInstanceOf[List[String]].reverse) {
            cudaMetadata.output.inputs ::= getOpLike(sym)
          }
          //output copy
          cudaMetadata.output.funcReturn = outList.tail.tail.tail.head
        }
        case err => mapNotFound(err)
      }
    }

    def fill(field: String) {
      val list = getFieldList(metadataMap, field)
      val data = cudaMetadata(field)
      data.func = list.head
      for (sym <- list.tail.head.asInstanceOf[List[String]].reverse) data.inputs ::= getOpLike(sym)
    }

    fill("gpuBlockSizeX") //threads/block - x
    fill("gpuBlockSizeY") //threads/block - y
    fill("gpuBlockSizeZ") //threads/block - z
    fill("gpuDimSizeX") //blocks in grid - x
    fill("gpuDimSizeY") //blocks in grid - y

  }

  def unsupportedType(err:String) = throw new RuntimeException("Unsupported Op Type found: " + err)
  def fieldNotFound(field: String, obj: Any) = throw new RuntimeException("Expecting field [" + field + "], found: " + obj )
  def mapNotFound(err:Any) = throw new RuntimeException("Expecting a Map object, found: " + err)
  def listNotFound(err:Any) = throw new RuntimeException("Expecting a List object, found: " + err)
  def opNotFound(op:String) = throw new RuntimeException("Couldn't find following op: " + op)
}


class DeliteTaskGraph {

  def getOp(id: String) = _ops(id)
  def registerOp(op: DeliteOP, overwrite: Boolean = false) {
    for (o <- op.getOutputs) {
      if (!overwrite && _ops.contains(o)) system.error("Output " + o + " (of Op " + op + ") is declared multiple times in DEG")
      _ops(o) = op
    }
  }

  val _ops = new HashMap[String, DeliteOP]
  var _result: DeliteOP = _

  var _version = 0.0
  var _kernelPath = ""

  def result : DeliteOP = _result
  def version: Double = _version
  def kernelPath: String = _kernelPath
  def ids: Iterable[String] = _ops.keys
  def ops: Iterable[DeliteOP] = _ops.values.toSet

}
