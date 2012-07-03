package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_While}
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.scheduler.OpList
import sync.ScalaSyncGenerator

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
    writeMethodHeader()

    //while condition
    if (whileLoop.predicateValue == "") {
      beginFunction()
      addKernelCalls(whileLoop.predicateGraph.schedule(location))
      writeOutput(whileLoop.predicateGraph.result._1, whileLoop.predicateGraph.result._2)
      endFunction()
      beginWhile(callFunction())
    }
    else {
      beginWhile(whileLoop.predicateValue)
    }

    //while body
    if (whileLoop.bodyValue == "") {
      addKernelCalls(whileLoop.bodyGraph.schedule(location))
    }
    endWhile()

    writeMethodFooter()
    writeFooter()

    addSource(out.toString)
  }

  protected def beginWhile(predicate: String)
  protected def endWhile()

  protected def beginFunction()
  protected def endFunction()
  protected def callFunction(): String

}

class ScalaWhileGenerator(val whileLoop: OP_While, val location: Int, val kernelPath: String)
  extends WhileGenerator with ScalaNestedGenerator with ScalaSyncGenerator {

  protected def beginWhile(predicate: String) {
    out.append("while (")
    out.append(predicate)
    out.append(") {\n")
  }

  protected def endWhile() {
    out.append("}\n")
  }

  protected def beginFunction() {
    out.append("def predicate(): Boolean = {\n")
  }

  protected def endFunction() {
    out.append("}\n")
  }

  protected def callFunction() = "predicate()"

  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)

  def executableName(location: Int) = "While_" + baseId + "_" + location

}
/*
class CudaGPUWhileGenerator(whileLoop: OP_While, location: Int) extends GPUWhileGenerator(whileLoop, location, Targets.Cuda) with CudaGPUExecutableGenerator {
  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    CudaMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaWhileGenerator(whileLoop, location, target).emitScala(syncList), executableName)
  }
}

class OpenCLGPUWhileGenerator(whileLoop: OP_While, location: Int) extends GPUWhileGenerator(whileLoop, location, Targets.OpenCL) with OpenCLGPUExecutableGenerator {
  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    OpenCLMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaWhileGenerator(whileLoop, location, target).emitScala(syncList), executableName)
  }
}

abstract class GPUWhileGenerator(whileLoop: OP_While, location: Int, target: Targets.Value) extends GPUNestedGenerator(whileLoop, location, target) {
  def makeExecutable(): Unit

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val inputOps = (whileLoop.predicateGraph.inputOps ++ whileLoop.bodyGraph.inputOps)
    val inputs = (whileLoop.predicateGraph.inputs ++ whileLoop.bodyGraph.inputs)
    implicit val aliases = new AliasTable[(DeliteOP,String)]

    writeFunctionHeader(out)
    val locations = whileLoop.nestedGraphs.flatMap(_.ops.map(_.scheduledResource)).toSet union Set(location)
    writeJNIInitializer(locations, out)

    val available = new ArrayBuffer[(DeliteOP,String)]
    val awaited = new ArrayBuffer[DeliteOP]
    //output predicate
    if (whileLoop.predicateValue == "") {
      available ++= inputs
      awaited ++= inputOps
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //write while
    if (whileLoop.predicateValue == "") {
      out.append("bool pred = ")
      out.append(getSymGPU(whileLoop.predicateGraph.result._2))
      out.append(";\n")
      out.append("while (pred")
    }
    else {
      out.append("while (")
      out.append(whileLoop.predicateValue)
    }
    out.append(") {\n")

    //output while body
    if (whileLoop.bodyValue == "") {
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputOps
      addKernelCalls(whileLoop.bodyGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //reevaluate predicate
    if (whileLoop.predicateValue == "") {
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputOps
      out.append("{\n")
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, available, awaited, new ArrayBuffer[DeliteOP], out) //dummy syncList b/c already added
      out.append("pred = ") //update var
      out.append(getSymGPU(whileLoop.predicateGraph.result._2))
      out.append(";\n}\n")
    }

    //print end of while and function
    out.append("} // end while loop\n")
    writeJNIFinalizer(locations, out)
    out.append("} // end While Function\n")

    out.toString
  }

  override protected def getScalaSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)

  protected def executableName = "While_" + baseId + "_"

}

class GPUScalaWhileGenerator(whileLoop: OP_While, location: Int, target: Targets.Value) extends GPUScalaNestedGenerator(whileLoop, location, target) {
  override protected def executableName = "While_" + baseId + "_"
  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)
}
*/
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
  def getSync(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = "Result_" + suffix(whileLoop, baseId, op, name)
}
