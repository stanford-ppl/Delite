package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.scheduler.OpList
import ppl.delite.runtime.graph.ops._
import sync._

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 4:09 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait WhileGenerator extends NestedGenerator {

  val whileLoop: OP_While
  val nested = whileLoop

  def makeExecutable() {
    updateOP()
    //header
    writeHeader()

    //while condition
    if (whileLoop.predicateValue == "") {
      beginFunction(whileLoop.getInputs)
      addKernelCalls(whileLoop.predicateGraph.schedule(location))
      writeReturn(false)
      writeOutput(whileLoop.predicateGraph.result._1, whileLoop.predicateGraph.result._2)
      endFunction()
      writeMethodHeader()
      beginWhile(callFunction(whileLoop.getInputs))
    }
    else {
      writeMethodHeader()
      beginWhile(whileLoop.predicateValue)
    }

    //while body
    if (whileLoop.bodyValue == "") {
      addKernelCalls(whileLoop.bodyGraph.schedule(location))
    }
    endWhile()

    writeMethodFooter()
    writeFooter()

    writeSyncObject()

    addSource(out.toString)
  }

  protected def beginWhile(predicate: String)
  protected def endWhile()

  protected def beginFunction(inputs: Seq[(DeliteOP,String)])
  protected def endFunction()
  protected def callFunction(inputs: Seq[(DeliteOP,String)]): String

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value) = {
    target match {
      case Targets.Scala => new ScalaWhileGenerator(whileLoop, location, graph) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Targets.Cpp => new CppWhileGenerator(whileLoop, location, graph) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + target.toString)
    }
  }
}

class ScalaWhileGenerator(val whileLoop: OP_While, val location: Int, val graph: DeliteTaskGraph)
  extends WhileGenerator with ScalaNestedGenerator with ScalaSyncGenerator {

  protected def beginWhile(predicate: String) {
    out.append("while (")
    out.append(predicate)
    out.append(") {\n")
  }

  protected def endWhile() {
    out.append("}\n")
  }

  protected def beginFunction(inputs: Seq[(DeliteOP,String)]) {
    out.append("def predicate(")
    writeInputs(inputs)
    out.append("): Boolean = {\n")
  }

  protected def endFunction() {
    out.append("}\n")
  }

  protected def callFunction(inputs: Seq[(DeliteOP,String)]) = {
    "predicate(" + inputs.map(i=>getSym(i._1,i._2)).mkString(",") + ")"
  }

  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)

  def executableName(location: Int) = "While_" + baseId + "_" + location

}

class CppWhileGenerator(val whileLoop: OP_While, val location: Int, val graph: DeliteTaskGraph)
  extends WhileGenerator with CppNestedGenerator with CppSyncGenerator {

  protected def beginWhile(predicate: String) {
    out.append("while (")
    out.append(predicate)
    out.append(") {\n")
  }

  protected def endWhile() {
    out.append("}\n")
  }

  protected def beginFunction(inputs: Seq[(DeliteOP,String)]) {
    out.append("bool predicate_")
    out.append(executableName(location))
    out.append("(")
    writeInputs(inputs)
    out.append(") {\n")
    val locationsRecv = nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Receive])).map(_.asInstanceOf[Receive].sender.from.scheduledResource).toSet
    val locations = if (nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Send])).nonEmpty) Set(location) union locationsRecv
                    else locationsRecv
    writeJNIInitializer(locations)
  }

  protected def endFunction() {
    out.append("}\n")
  }

  protected def callFunction(inputs: Seq[(DeliteOP,String)]) = {
    "predicate_" + executableName(location) + "(" + (resourceInfoSym+:inputs.map(i=>getSymHost(i._1,i._2))).mkString(",") + ")"
  }

  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)

  def executableName(location: Int) = "While_" + baseId + "_" + location


}

class CudaWhileGenerator(val whileLoop: OP_While, val location: Int, val graph: DeliteTaskGraph)
  extends WhileGenerator with CudaNestedGenerator with CudaSyncGenerator {

  protected def beginWhile(predicate: String) {
    out.append("while (")
    out.append(predicate)
    out.append(") {\n")
  }

  protected def endWhile() {
    out.append("}\n")
  }

  protected def beginFunction(inputs: Seq[(DeliteOP,String)]) {
    out.append("bool predicate_")
    out.append(executableName(location))
    out.append("(")
    writeInputs(inputs)
    out.append(") {\n")
    val locationsRecv = nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Receive])).map(_.asInstanceOf[Receive].sender.from.scheduledResource).toSet
    val locations = if (nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Send])).nonEmpty) Set(location) union locationsRecv
                    else locationsRecv
    writeJNIInitializer(locations)
  }

  protected def endFunction() {
    out.append("}\n")
  }

  protected def callFunction(inputs: Seq[(DeliteOP,String)]) = {
    "predicate_" + executableName(location) + "(" + inputArgs(whileLoop) + ")"
  }

  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)

  def executableName(location: Int) = "While_" + baseId + "_" + location


}

private[codegen] object WhileCommon {
  private def suffix(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = {
    if (whileLoop.predicateGraph.ops.contains(op))
      "x" + baseId + "P_" + name
    else if (whileLoop.bodyGraph.ops.contains(op))
      "x" + baseId + "B_" + name
    else //input
      "x"  + baseId + "_" + name
  }

  def getSym(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = "x" + suffix(whileLoop, baseId, op, name)
  def getSync(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = "Result_" + op.id + "_" + suffix(whileLoop, baseId, op, name)
}
