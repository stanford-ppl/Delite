package ppl.delite.runtime.graph

import java.io.File
import _root_.scala.util.parsing.json.JSON
import ops._
import targets._
import ppl.delite.runtime.Config
import ppl.delite.runtime.profiler.Profiler
import ppl.delite.runtime.scheduler.PartialSchedule
import collection.mutable.{HashSet, HashMap}

object DeliteTaskGraph {

  //implicit conversion to strings
  implicit def anyToString(obj: Any): String = obj.toString

  def apply(degFile: File) = {
    val contents = scala.io.Source.fromFile(degFile).mkString
    import JSON._

    phrase(root)(new lexical.Scanner(contents)) match {
      case Success(result,_) => buildFromParsedJSON(resolveType(result))
      case f@NoSuccess(_,_) => throw new RuntimeException("Couldn't parse the DEG file:\n" + f.toString)
    }
  }

  def buildFromParsedJSON(json: Any) = {
    implicit val graph = new DeliteTaskGraph
    json match {
      case degm: Map[Any,Any] => try { parseDEGMap(degm) } catch { case e: Exception => e.printStackTrace; throw e; }
      case err@_ => mapNotFound(err)
    }
    graph
  }

  def parseDEGMap(degm: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val deg = getFieldMap(degm, "DEG")
    graph._version = getFieldDouble(deg, "version")
    graph._kernelPath = getFieldString(deg, "kernelpath")
    graph._appName = getFieldString(deg, "name")
    parseOps(getFieldList(deg, "ops"))
    graph._targets = graph.totalOps.filter(o => !o.isInstanceOf[Arguments]).flatMap(o => o.getOutputTypesMap.keySet)
  }

  def parseOps(ops: List[Any])(implicit graph: DeliteTaskGraph) {
    for(_op <- ops) {
      val op = _op.asInstanceOf[Map[Any, Any]]
      val opType = getFieldString(op, "type")
      opType match {
        case "SingleTask" => processCommon(op, "OP_Single")
        case "External" => processCommon(op, "OP_External")
        case "Input" => processCommon(op, "OP_FileReader")
        case "MultiLoop" => processCommon(op, "OP_MultiLoop")
        case "Foreach" => processCommon(op, "OP_Foreach")
        case "Conditional" => processIfThenElseTask(op)
        case "WhileLoop" => processWhileTask(op)
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

  def getFieldMapOption(map: Map[Any, Any], field: String): Option[Map[Any, Any]] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any,Any] => Some(map)
        case err@_ => None
      }
      case None => None
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

  def getOp(sym: String)(implicit graph: DeliteTaskGraph) = {
    graph._ops.get(sym) match {
      case Some(op) => op
      case None => {
        val in = findOp(sym)(graph.superGraph)
        graph._inputs.getOrElseUpdate(sym, new OP_Input(in))
      }
    }
  }

  def findOp(sym: String)(implicit firstGraph: DeliteTaskGraph): DeliteOP = {
    var graph = firstGraph
    while (graph != null) {
      graph._ops.get(sym) match {
        case Some(op) => return op
        case None => //continue
      }
      graph = graph._superGraph
    }
    opNotFound(sym)
  }

  def processCommon(op: Map[Any, Any], opType: String)(implicit graph: DeliteTaskGraph) {
    val id = getFieldString(op, "kernelId")

    val resultMap = processOutputTypes(op)

    // source context
    val (fileName, line, opName) = getFieldMapOption(op, "sourceContext") match {
      case None => ("<unknown file>", 0, id)
      case Some(sourceContext) =>
        (getFieldString(sourceContext, "fileName"), getFieldString(sourceContext, "line").toInt, getFieldString(sourceContext, "opName"))
    }

    // TODO: maybe it would be better to add source info to DeliteOP?
    Profiler.sourceInfo += (id -> (fileName, line, opName))
	
    val newop = opType match {
      case "OP_Single" => new OP_Single(id, "kernel_"+id, resultMap)
      case "OP_External" => new OP_External(id, "kernel_"+id, resultMap)
      case "OP_FileReader" => new OP_FileReader(id, "kernel_"+id, resultMap)
      case "OP_MultiLoop" =>
        val size = getFieldString(op, "sizeValue")
        val sizeIsConst = getFieldString(op, "sizeType") == "const"
        val numDynamicChunks = getFieldString(op, "numDynamicChunksValue")
        val numDynamicChunksIsConst = getFieldString(op, "numDynamicChunksType") == "const"
        if (resultMap(Targets.Scala).values.contains("Unit")) //FIXME: handle foreaches
          if (Config.clusterMode == 1) println("WARNING: ignoring stencil of op with Foreach: " + id)
        else
          processStencil(op)
        new OP_MultiLoop(id, size, sizeIsConst, numDynamicChunks, numDynamicChunksIsConst, "kernel_"+id, resultMap, getFieldBoolean(op, "needsCombine"), getFieldBoolean(op, "needsPostProcess"))
      case "OP_Foreach" => new OP_Foreach(id, "kernel_"+id, resultMap)
      case other => error("OP Type not recognized: " + other)
    }

    //handle supported targets
    for (t <- getFieldList(op, "supportedTargets")) {
      newop.supportedTargets += Targets(t)
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
      newop.addMutableInput(mutable, m)
    }

    //handle anti dependencies
    val antiDeps = getFieldList(op, "antiDeps")
    for(a <- antiDeps) {
      val antiDep = getOp(a)
      newop.addDependency(antiDep)
      antiDep.addConsumer(newop)
      newop.addAntiDep(antiDep)
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
    graph._result = (newop, newop.getOutputs.head)

    //process target GPU metadata
    for (tgt <- Targets.GPU) {
      if (newop.supportsTarget(tgt)) processGPUMetadata(op, newop, tgt)
    }

  }

  def processOutputTypes(op: Map[Any,Any]) = {
    val outputs = getFieldList(op, "outputs")
    val outputTypes = getFieldMap(op, "output-types")
    var resultMap = Map[Targets.Value,Map[String,String]]()
    for (target <- Targets.values) {
      var outputMap = Map[String,String]()
      for (output <- outputs if getFieldMap(outputTypes, output) contains target.toString) {
        outputMap += output.toString -> getFieldString(getFieldMap(outputTypes, output), target.toString)
      }
      if (getFieldList(op, "supportedTargets") contains target.toString) {
        if (op contains "return-types") { //multiple outputs must return via a container type
          outputMap += "functionReturn" -> getFieldString(getFieldMap(op, "return-types"), target.toString)
        }
        else {
          outputMap += "functionReturn" -> outputMap.head._2 //single output can return directly
        }
      }
      if (outputMap.size > 0) resultMap += target -> outputMap
    }
    resultMap
  }

  //FIXME: should be based on compiler IR node type rather than just output type
  def isPrimitiveType(tp: String) = tp match {
    case "Int" | "Long" | "Float" | "Double" | "Char" | "Short" | "Byte" | "Boolean" | "Unit" => true
    case _ => false
  }

  def processStencil(op: Map[Any,Any])(implicit graph: DeliteTaskGraph) = {
    val stencilMap = getFieldMap(op, "stencil")
    for (in <- getFieldList(op, "inputs")) {
      val localStencil = Stencil(getFieldString(stencilMap, in))
      traverseInputs(findOp(in), in)

      def updateStencil(op: DeliteOP, in: String) {
        if (localStencil != Empty && (op.isInstanceOf[OP_FileReader] || op.isInstanceOf[OP_MultiLoop]))
          if (Config.clusterMode == 1) println("adding " + localStencil + " to output " + in + " of op " + op)
        val globalStencil = op.stencilMap
        if (globalStencil contains in) globalStencil(in) = globalStencil(in) combine localStencil
        else globalStencil(in) = localStencil
      }

      //alias propagation, seems very sketchy...
      def traverseInputs(op: DeliteOP, in: String)(implicit graph: DeliteTaskGraph) {
        if (!isPrimitiveType(op.outputType)) {
          updateStencil(op, in)
          op match {
            case o:OP_Single => 
              o.getInputs.foreach(i => traverseInputs(findOp(i._2), i._2))
            case o:OP_Condition => 
              val resT = o.thenGraph.result._2
              if (resT != null) traverseInputs(findOp(resT)(o.thenGraph), resT)(o.thenGraph)
              val resE = o.elseGraph.result._2
              if (resE != null) traverseInputs(findOp(resE)(o.elseGraph), resE)(o.elseGraph)
            case _ => 
          }
        }
      }
    }
  }

  def combineTypesMap(inMaps: List[Map[Targets.Value,Map[String,String]]]): Map[Targets.Value, Map[String,String]] = {
    val targetSet = inMaps.flatMap(_.keySet)
    targetSet.map(target => (target,inMaps.flatMap(_.get(target)).reduceLeft(_ ++ _))).toMap
  }

  def newGraph(implicit graph: DeliteTaskGraph) = {
    val newGraph = new DeliteTaskGraph
    newGraph._version = graph._version
    newGraph._kernelPath = graph._kernelPath
    newGraph._appName = graph._appName
    newGraph._superGraph = graph
    newGraph
  }

  def parseSubGraph(op: Map[Any,Any], prefix: String)(implicit graph: DeliteTaskGraph) = {
    val subGraph = newGraph
    var value = ""
    if (getFieldString(op, prefix+"Type") == "const")
      value = getFieldString(op, prefix+"Value")
    else {
      parseOps(getFieldList(op, prefix+"Ops"))(subGraph)
      op.get(prefix+"Output") match {
        case Some(sym) => sym match {
          case "()" => //if Unit literal, ignore
          case _ => subGraph._result = (getOp(sym)(subGraph), sym)
        }
        case None =>
      }
    }
    (subGraph, value)
  }

  def processReturnTypes(op: Map[Any, Any], id: String) = {
    val outputTypes = getFieldMap(op, "return-types")
    var resultMap = Map[Targets.Value, Map[String,String]]()
    for (target <- Targets.values) {
      if (outputTypes contains target.toString)
        resultMap += target -> Map(id -> outputTypes(target.toString), "functionReturn" -> outputTypes(target.toString))
    }
    resultMap
  }

  def processIfThenElseTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")

    val (predGraph, predValue) = parseSubGraph(op, "cond")
    val (thenGraph, thenValue) = parseSubGraph(op, "then")
    val (elseGraph, elseValue) = parseSubGraph(op, "else")

    val resultMap = processReturnTypes(op, id)
    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var ifDeps = Set.empty[DeliteOP]
    for (depId <- depIds) ifDeps += getOp(depId)
    ifDeps ++= (predGraph._inputs.keySet ++ thenGraph._inputs.keySet ++ elseGraph._inputs.keySet) map { getOp(_) }
    val ifAntiDeps = getFieldList(op, "antiDeps").map(getOp(_)).toSet

    //find inputs at nesting level of the IfThenElse
    val internalOps = (predGraph.ops ++ thenGraph.ops ++ elseGraph.ops).toSet
    var ifInputs = for (in <- internalOps; (op,sym) <- in.getInputs; if (op.isInstanceOf[OP_Input])) yield (getOp(sym), sym)
    ifInputs ++= (for ((op,sym) <- Seq(predGraph.result, thenGraph.result, elseGraph.result); if (op.isInstanceOf[OP_Input])) yield (getOp(sym), sym))
    val ifMutableInputs = for (in <- internalOps; (op,sym) <- in.getMutableInputs; if (op.isInstanceOf[OP_Input])) yield (getOp(sym), sym)

    val conditionOp = new OP_Condition(id, resultMap, predGraph, predValue, thenGraph, thenValue, elseGraph, elseValue)
    conditionOp.dependencies = ifDeps
    conditionOp.inputList = ifInputs.toList
    conditionOp.mutableInputs = ifMutableInputs
    conditionOp.antiDeps = ifAntiDeps

    if (predValue == "") {
      for (tgt <- Targets.GPU) extractGPUMetadata(conditionOp, predGraph, graph, tgt)
    }
    if (thenValue == "") {
      for (tgt <- Targets.GPU) extractGPUMetadata(conditionOp, thenGraph, graph, tgt)
      val resultMetadata = conditionOp.getGPUMetadata(Targets.Cuda).outputs.filter(o=>o._2==thenGraph.result._2)
      if (resultMetadata.length == 1) conditionOp.getGPUMetadata(Targets.Cuda).outputs ++= List((resultMetadata(0)._1,id))
    }
    if (elseValue == "") {
      for (tgt <- Targets.GPU) extractGPUMetadata(conditionOp, elseGraph, graph, tgt)
      val resultMetadata = conditionOp.getGPUMetadata(Targets.Cuda).outputs.filter(o=>o._2==elseGraph.result._2)
      if (resultMetadata.length == 1) conditionOp.getGPUMetadata(Targets.Cuda).outputs ++= List((resultMetadata(0)._1,id))
    }

    //add consumer edges
    for(dep <- ifDeps)
      dep.addConsumer(conditionOp)

    //add to graph
    graph.registerOp(conditionOp)
    graph._result = (conditionOp, conditionOp.getOutputs.head)
  }

  def processWhileTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    // get id
    val id = getFieldString(op,"outputId")

    val (predGraph, predValue) = parseSubGraph(op, "cond")
    val (bodyGraph, bodyValue) = parseSubGraph(op, "body")

    val depIds = getFieldList(op, "controlDeps") ++ getFieldList(op, "antiDeps")
    var whileDeps = Set.empty[DeliteOP]
    for (depId <- depIds) whileDeps += getOp(depId)
    whileDeps ++= (predGraph._inputs.keySet ++ bodyGraph._inputs.keySet) map { getOp(_) }
    val whileAntiDeps = getFieldList(op, "antiDeps").map(getOp(_)).toSet

    //find inputs at nesting level of the While
    val internalOps = (predGraph.ops ++ bodyGraph.ops).toSet
    var whileInputs = for (in <- internalOps; (op,sym) <- in.getInputs; if (op.isInstanceOf[OP_Input])) yield (getOp(sym), sym)
    whileInputs ++= (for ((op,sym) <- Seq(predGraph.result, bodyGraph.result); if (op.isInstanceOf[OP_Input])) yield (getOp(sym), sym))
    val whileMutableInputs = for (in <- internalOps; (op,sym) <- in.getMutableInputs; if (op.isInstanceOf[OP_Input])) yield (getOp(sym), sym)

    val whileOp = new OP_While(id, predGraph, predValue, bodyGraph, bodyValue)
    whileOp.dependencies = whileDeps
    whileOp.inputList = whileInputs.toList
    whileOp.mutableInputs = whileMutableInputs
    whileOp.antiDeps = whileAntiDeps

    if (predValue == "") {
      for (tgt <- Targets.GPU) extractGPUMetadata(whileOp, predGraph, graph, tgt)
    }
    if (bodyValue == "") {
      for (tgt <- Targets.GPU) extractGPUMetadata(whileOp, bodyGraph, graph, tgt)
    }

    //add consumer edges
    for (dep <- whileDeps)
      dep.addConsumer(whileOp)

    //add to graph
    graph.registerOp(whileOp)
    graph._result = (whileOp, whileOp.getOutputs.head)
  }

  // Change Metadata apply method from Target -> specific metadata in DeliteOP
  def extractGPUMetadata(superOp: OP_Nested, innerGraph: DeliteTaskGraph, outerGraph: DeliteTaskGraph, tgt: Targets.Value) {
    superOp.getGPUMetadata(tgt).outputs ++= innerGraph.result._1.getGPUMetadata(tgt).outputs
    /*
    for (op <- innerGraph._ops.values; key <- op.getGPUMetadata(tgt).inputs.keys) {
      try {
        val inOp = getOp(key._2)(outerGraph)
        superOp.getGPUMetadata(tgt).inputs += (inOp, key._2) -> op.getGPUMetadata(tgt).inputs(key)
      }
      catch {
        case e => //symbol only exists in inner graph, therefore ignore
      }
    }
    */
  }

  /**
   * Add the Arguments op to the task graph
   * This op feeds all application ops that consume command line / external arguments
   * By definition it has no dependencies
   */
  def processArgumentsTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val id = getFieldString(op, "kernelId")
    val argIdx = getFieldString(op, "index").toInt
    val argTypes = processReturnTypes(op, id)
    val args = new Arguments(id, argIdx, argTypes)
    args.supportedTargets ++= argTypes.keySet
    graph.registerOp(args)
    graph._result = (args, id)
  }

  /**
   * Add the EOP op to the task graph
   * This op follows the application result
   */
  def processEOPTask(op: Map[Any, Any])(implicit graph: DeliteTaskGraph) {
    val id = "eop"
    val resultTypes = processReturnTypes(op, id)
    val value = getFieldString(op, "Value")
    val tpe = getFieldString(op, "Type")
    
    val EOP = new EOP(id, resultTypes, (tpe,value))
    if (tpe == "symbol") {
      val result = getOp(value)
      EOP.addInput(result, value)
      EOP.addDependency(result)
      result.addConsumer(EOP)
    }
    else { //const output, but EOP still needs to depend on result of application
      val result = graph._result._1
      EOP.addDependency(result) 
      result.addConsumer(EOP)
    }
    
    graph.registerOp(EOP)
    graph._result = (EOP, EOP.id)
  }

  /**
   * Extract the required GPU (Cuda/OpenCL) metadata from the DEG
   */
  def processGPUMetadata(op: Map[Any, Any], newop: DeliteOP, tgt: Targets.Value)(implicit graph: DeliteTaskGraph) {
    val metadataAll = getFieldMap(op, "metadata")
    val metadataMap = getFieldMap(metadataAll, tgt.toString)
    val metadata = newop.getGPUMetadata(tgt)
  
    //TODO: Add feature to unwrap/wrap OpenCL datastructures
    /*
    for (input <- getFieldList(metadataMap, "gpuInputs").reverse) { //input list
      //tgt match {
      //  case Targets.OpenCL => data.objFields = value.tail.head.asInstanceOf[Map[String,String]]
      //  case _ =>
      //}
    }
    */
    for (temp <- getFieldList(metadataMap, "gpuTemps").reverse) {
      val tempMap = temp.asInstanceOf[Map[String,Any]]
      val sym = tempMap.keys.head
      val value = tempMap.values.head.asInstanceOf[List[String]]
      val data = metadata.newTemp(sym,value(0),value(1))
    }

    //output allocation
    val outputs = getFieldMap(metadataMap, "gpuOutputs").asInstanceOf[Map[String,Map[Any,Any]]]
    for ((osym,odata) <- outputs) {
      val elemType = getFieldString(odata, "elemType")
      val types = getFieldMap(odata, "types").asInstanceOf[Map[String,String]]
      val funcs = getFieldMap(odata, "funcs").asInstanceOf[Map[String,List[String]]]
      metadata.newOutput(osym,elemType,types,funcs)
      //tgt match {
      //  case Targets.OpenCL => output.objFields = outList.tail.tail.tail.tail.head.asInstanceOf[Map[String,String]]
      //  case _ =>
      //}
    }

    //aux meta
    val aux = getFieldMap(metadataMap, "aux").asInstanceOf[Map[Any,Any]]
    if(aux.get("multiDim").isDefined) {
      for (m <- getFieldList(aux,"multiDim").asInstanceOf[List[Map[Any,Any]]]) {
        val level = getFieldString(m,"level").toInt
        val dim = getFieldString(m,"dim")
        val size = getFieldString(m,"size").toInt
        val spanMap = getFieldMap(m,"span").asInstanceOf[Map[Any,Any]]
        val spanTpe = getFieldString(spanMap,"tpe")
        val spanSize = getFieldString(spanMap,"size")
        metadata.mapping.append(Mapping(level,dim,size,spanTpe,spanSize))
      }
    }

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
      if (!overwrite && _ops.contains(o)) error("Output " + o + " / Op " + op + " is declared multiple times in DEG")
      _ops(o) = op
    }
  }

  protected var _version = 0.0
  protected var _targets: Set[Targets.Value] = null
  protected var _kernelPath = ""
  protected var _appName = ""

  protected val _ops = new HashMap[String, DeliteOP]
  protected val _inputs = new HashMap[String, OP_Input]
  protected var _result: (DeliteOP, String) = (null, null)
  protected var _superGraph: DeliteTaskGraph = null

  var schedule: PartialSchedule = _

  def result: (DeliteOP, String) = _result
  def version: Double = _version
  def targets: Set[Targets.Value] = _targets
  def kernelPath: String = _kernelPath
  def appName: String = _appName
  def superGraph: DeliteTaskGraph = _superGraph
  def symbols: Set[String] = _ops.keys.toSet
  def ops: Set[DeliteOP] = _ops.values.toSet
  def inputOps: Set[DeliteOP] = _inputs.values.toSet
  def inputs: Set[(OP_Input,String)] = _inputs.toSet[(String,OP_Input)].map(i=>Pair(i._2,i._1))

  def replaceOp(old: DeliteOP, op: DeliteOP) {
    //update ops
    for (dep <- old.getDependencies) dep.replaceConsumer(old, op)
    for (c <- old.getConsumers) {
      c.replaceDependency(old, op)
      for ((x,sym) <- c.getInputs; if (x == old)) {
        c.replaceInput(old, op, sym)
      }
    }

    //update graph
    if (_result._1 == old) _result = (op, _result._2)
    registerOp(op, true)

    //clear op
    old.dependencies = Set.empty
    old.consumers = Set.empty
    old.inputList = Nil
    old.mutableInputs = Set.empty
    old.antiDeps = Set.empty
  }

  def totalOps: Set[DeliteOP] = {
    _ops.values.flatMap(_ match {
      case m: OP_Nested => m.nestedGraphs.flatMap(g => g.totalOps)
      case o@_ => Seq(o)
    }).toSet ++ inputOps
  }
}
