package ppl.delite.runtime.graph

import java.io.File
import _root_.scala.util.parsing.json.JSON
import ops._
import collection.mutable.HashMap
import targets._
import ppl.delite.runtime.scheduler.PartialSchedule

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
        case "SubGraph" => processSubGraph(op)
        case "Arguments" => processArgumentsTask(op)
        case "EOP" => processEOPTask(op) //end of program
        case "EOG" => //end of nested graph, do nothing
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

  def getOp(op: String)(implicit graph: DeliteTaskGraph) = {
    graph._ops.get(op) match {
      case Some(o) => o
      case None => {
        val in = findOp(op)
        graph._inputs.getOrElseUpdate(in.id, new OP_Input(in))
      }
    }
  }

  def findOp(op: String)(implicit firstGraph: DeliteTaskGraph): DeliteOP = {
    var graph = firstGraph.superGraph
    while (graph != null) {
      graph._ops.get(op) match {
        case Some(op) => return op
        case None => //continue
      }
      graph = graph._superGraph
    }
    opNotFound(op)
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
      println("plain output: "+newop+" "+outputs)
    }

    //handle inputs
    val inputs = getFieldList(op, "inputs")
    for(i <- inputs.reverse) {
      val input = getOp(i)
      newop.addInput(input, i)
      newop.addDependency(input)
      input.addConsumer(newop)
    }

    //handle mutable inputs
    val mutableInputs = getFieldList(op, "mutableInputs")
    for (m <- mutableInputs) {
      val mutable = getOp(m)
      newop.addMutableInput(mutable)
    }

    //handle anti dependencies
    val antiDeps = getFieldList(op, "antiDeps")
    for(a <- antiDeps) {
      val antiDep = getOp(a)
      newop.addDependency(antiDep)
      antiDep.addConsumer(newop)
    }

    //handle control dependencies
    val controlDeps = getFieldList(op, "controlDeps")
    for(c <- controlDeps) {
      val controlDep = getOp(c)
      newop.addDependency(controlDep)
      controlDep.addConsumer(newop)
    }

    //add new op to graph list of ops
    graph.registerOp(newop)
    //if (graph._ops.contains(id)) error("Op " + id + " is declared multiple times in DEG")
    //graph._ops += id -> newop

    //process target metadata
    if (resultMap.contains(Targets.Cuda)) processCudaMetadata(op, newop)

    //process kernel variant
    op.get("variant") match {
      case None => //do nothing
      case Some(field) => field match {
        case map: Map[Any,Any] => newop.variant = processVariant(newop, resultMap, map)
        case err => mapNotFound(err)
      }
    }

    //last op will be result op
    graph._result = newop

  }

  def newGraph(implicit graph: DeliteTaskGraph) = {
    val newGraph = new DeliteTaskGraph
    newGraph._version = graph._version
    newGraph._kernelPath = graph._kernelPath
    newGraph._superGraph = graph
    newGraph
  }

  def processVariant(op: DeliteOP, resultType: Map[Targets.Value,String], graph: Map[Any, Any])(implicit outerGraph: DeliteTaskGraph) = {
    val varGraph = newGraph
    parseOps(getFieldList(graph, "ops"))(varGraph)
    if (getFieldString(graph, "outputType") == "symbol")
      varGraph._result = getOp(getFieldString(graph, "outputValue"))(varGraph)
    else
      assert(getFieldString(graph, "outputValue") == "()") //only const a variant should return is a Unit literal

    val v = new OP_Variant(op.id, resultType, op, varGraph)

    v.inputSyms = op.getInputs.map(d => getOp(d._2)(varGraph)).toList
    extractCudaMetadata(v, varGraph, outerGraph)
    v
  }

  def parseSubGraph(op: Map[Any,Any], prefix: String)(implicit graph: DeliteTaskGraph) = {
    val subGraph = newGraph
    var value = ""
    if (getFieldString(op, prefix+"Type") == "const")
      value = getFieldString(op, prefix+"Value")
    else {
      parseOps(getFieldList(op, prefix+"Ops"))(subGraph)
      op.get(prefix+"Output") match {
        case Some(field) => field match {
          case "()" => //if Unit literal, ignore
          case _ => subGraph._result = getOp(field)(subGraph)
        }
        case None =>
      }
    }
    (subGraph, value)
  }

  def processIfThenElseTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")

    val (predGraph, predValue) = parseSubGraph(op, "cond")
    val (thenGraph, thenValue) = parseSubGraph(op, "then")
    val (elseGraph, elseValue) = parseSubGraph(op, "else")

    val outputTypes = getFieldMap(op, "return-types")
    var resultMap = Map[Targets.Value, String]()
    for (target <- Targets.values) {
      if (outputTypes.contains(target.toString))
        resultMap += target -> outputTypes(target.toString)
    }

    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var ifDeps: List[DeliteOP] = Nil
    for (depId <- depIds) ifDeps ::= getOp(depId)

    //list of all dependencies of the if block, minus any dependencies within the block (predicate could be an input or internal)
    val internalOps = (predGraph.ops ++ thenGraph.ops ++ elseGraph.ops).toList
    ifDeps = resolveInputs((predGraph.result :: ifDeps ++ internalOps.flatMap(_.getDependencies)) filterNot { internalOps contains })
    val ifInputs = resolveInputs((predGraph.result :: internalOps.flatMap(_.getInputs.map(_._1))) filterNot { internalOps contains }).distinct //TR FIXME
    val ifInputSyms = resolveInputs(ifInputs)(predGraph)
    val ifMutableInputs = resolveInputs((internalOps.flatMap(_.getMutableInputs)) filterNot { internalOps contains })

    val conditionOp = new OP_Condition(id, resultMap, predGraph, predValue, thenGraph, thenValue, elseGraph, elseValue)
    conditionOp.dependencyList = ifDeps
    conditionOp.inputList = ifInputs.map(op => (op, op.getOutputs.head)) //TR FIXME
    conditionOp.outputList = List(id) //TR FIXME
    conditionOp.mutableInputList = ifMutableInputs
    conditionOp.inputSyms = ifInputSyms

    if (predValue == "") extractCudaMetadata(conditionOp, predGraph, graph)
    if (thenValue == "") extractCudaMetadata(conditionOp, thenGraph, graph)
    if (elseValue == "") extractCudaMetadata(conditionOp, elseGraph, graph)

    //add consumer edges
    for(dep <- ifDeps)
      dep.addConsumer(conditionOp)

    //add to graph
    // TODO: fix up outputs/outputSlotTypes and use graph.registerOp
    graph._ops += id -> conditionOp
    graph._result = conditionOp
  }

  //translate ops (ids) from one scope to another
  def resolveInputs(deps: List[DeliteOP])(implicit graph: DeliteTaskGraph) = deps map { d => getOp(d.id) } //TR TODO

  def processWhileTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")

    val (predGraph, predValue) = parseSubGraph(op, "cond")
    val (bodyGraph, bodyValue) = parseSubGraph(op, "body")

    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var whileDeps: List[DeliteOP] = Nil
    for (depId <- depIds) whileDeps ::= getOp(depId)

    //list of all dependencies of the while block, minus any dependencies within the block (predicate could be an input or internal)
    val internalOps = (predGraph.ops ++ bodyGraph.ops).toList
    whileDeps = resolveInputs((predGraph.result :: whileDeps ++ internalOps.flatMap(_.getDependencies)) filterNot { internalOps contains })
    val whileInputs = resolveInputs((predGraph.result :: internalOps.flatMap(_.getInputs.map(_._1))) filterNot { internalOps contains }).distinct //TR FIXME
    val whileInputSyms = resolveInputs(whileInputs)(predGraph)
    val whileMutableInputs = resolveInputs((internalOps.flatMap(_.getMutableInputs)) filterNot { internalOps contains })

    val whileOp = new OP_While(id, predGraph, predValue, bodyGraph, bodyValue)
    whileOp.dependencyList = whileDeps
    whileOp.inputList = whileInputs.map(op => (op, op.getOutputs.head)) //TR FIXME
    whileOp.outputList = List(id) //TR FIXME
    whileOp.mutableInputList = whileMutableInputs
    whileOp.inputSyms = whileInputSyms

    if (predValue == "") extractCudaMetadata(whileOp, predGraph, graph)
    if (bodyValue == "") extractCudaMetadata(whileOp, bodyGraph, graph)

    //add consumer edges
    for (dep <- whileDeps)
      dep.addConsumer(whileOp)

    //add to graph
    graph._ops += id -> whileOp
    graph._result = whileOp
  }

  //TODO: this, while, and if can probably be factored
  def processSubGraph(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")
    val (bodyGraph, bodyValue) = parseSubGraph(op, "")
    assert(bodyValue == "")

    var resultMap = Map[Targets.Value, String]()
    for (target <- Targets.values) {
      if (bodyGraph.result.supportsTarget(target))
        resultMap += target -> bodyGraph.result.outputType(target)
    }

    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var graphDeps: List[DeliteOP] = Nil
    for (depId <- depIds) graphDeps ::= getOp(depId)

    //list of all dependencies of the block, minus any dependencies within the block
    val internalOps = bodyGraph.ops.toList
    graphDeps = resolveInputs((graphDeps ++ internalOps.flatMap(_.getDependencies)) filterNot { internalOps contains })
    val graphInputs = resolveInputs((internalOps.flatMap(_.getInputs.map(_._1))) filterNot { internalOps contains }).distinct //TR FIXME
    val graphInputSyms = resolveInputs(graphInputs)(bodyGraph)
    val graphMutableInputs = resolveInputs((internalOps.flatMap(_.getMutableInputs)) filterNot { internalOps contains })

    val graphOp = new OP_Variant(id, resultMap, null, bodyGraph)
    graphOp.dependencyList = graphDeps
    graphOp.inputList = graphInputs.map(op => (op, op.getOutputs.head)) //TR FIXME
    graphOp.outputList = List(id) //TR FIXME
    graphOp.mutableInputList = graphMutableInputs
    graphOp.inputSyms = graphInputSyms

    extractCudaMetadata(graphOp, bodyGraph, graph)

    //add consumer edges
    for (dep <- graphDeps)
      dep.addConsumer(graphOp)

    //add to graph
    // TODO: fix up outputs/outputSlotTypes and use graph.registerOp
    graph._ops += id -> graphOp
    graph._result = graphOp
  }

  def extractCudaMetadata(superOp: OP_Nested, innerGraph: DeliteTaskGraph, outerGraph: DeliteTaskGraph) {
    superOp.cudaMetadata.output = innerGraph.result.cudaMetadata.output
    for (op <- innerGraph._ops.values; key <- op.cudaMetadata.inputs.keys) {
      try {
        val inOp = getOp(key.id)(outerGraph)
        superOp.cudaMetadata.inputs += inOp -> op.cudaMetadata.inputs(key)
      }
      catch {
        case e => //symbol only exists in inner graph, therefore ignore
      }
    }
  }

  /**
   * Add the Arguments op to the task graph
   * This op feeds all application ops that consume command line arguments
   * By definition it has no dependencies
   */
  def processArgumentsTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val id = getFieldString(op, "kernelId")
    Arguments.id = id
    graph._ops += id -> Arguments
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
    graph._ops += "EOP" -> EOP
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
      val inputMap = input.asInstanceOf[Map[String,Any]]
      val id = inputMap.keys.head
      val value = inputMap.values.head.asInstanceOf[List[Any]]
      val data = cudaMetadata.newInput(getOp(id))
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

    def getOpLike(sym: String) = {
      try getOp(sym)
      catch { case _ => tempSyms(sym) }
    }

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

  def registerOp(op: DeliteOP, overwrite: Boolean = false) {
    for (o <- op.getOutputs) {
      if (!overwrite && _ops.contains(o)) println("WARNING: Output " + o + "/Op " + op + " is declared multiple times in DEG")
      //else 
      _ops(o) = op
    }
  }

  var _version = 0.0
  var _kernelPath = ""

  val _ops = new HashMap[String, DeliteOP]
  val _inputs = new HashMap[String, OP_Input]
  var _result: DeliteOP = _
  var _superGraph: DeliteTaskGraph = null

  var schedule: PartialSchedule = _

  def result : DeliteOP = _result
  def version: Double = _version
  def kernelPath: String = _kernelPath
  def superGraph: DeliteTaskGraph = _superGraph
  def ids: Iterable[String] = _ops.keys
  def ops: Iterable[DeliteOP] = _ops.values.toSet
  def inputs: Iterable[OP_Input] = _inputs.values

  def replaceOp(old: DeliteOP, op: DeliteOP) {
    //update ops
    for (dep <- old.getDependencies) dep.replaceConsumer(old, op)
    for (c <- old.getConsumers) {
      c.replaceDependency(old, op)
      if (c.getInputs contains old) c.replaceInput(old, op, op.id) //TR FIXME
      c.cudaMetadata.replaceInput(old, op)
    }

    //update graph
    if (_result == old) _result = op
    _ops -= old.id
    _ops += op.id -> op

    //clear op
    old.dependencyList = Nil
    old.consumerList = Nil
    old.inputList = Nil
    old.outputList = Nil
    old.mutableInputList = Nil
  }
}
