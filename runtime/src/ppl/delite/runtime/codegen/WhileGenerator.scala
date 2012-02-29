package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_While}
import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 4:09 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class WhileGenerator(whileLoop: OP_While, location: Int) extends NestedGenerator(whileLoop, location) {

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val inputs = (whileLoop.predicateGraph.inputOps ++ whileLoop.bodyGraph.inputOps)

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    //output predicate
    if (whileLoop.predicateValue == "") {
      available ++= inputs
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, out, available, syncList)
    }

    //write while
    if (whileLoop.predicateValue == "") {
      out.append("var pred: Boolean = ")
      out.append(getSym(whileLoop.predicateGraph.result._1, whileLoop.predicateGraph.result._2))
      out.append('\n')
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
      addKernelCalls(whileLoop.bodyGraph.schedule(location), location, out, available, syncList)
    }

    //reevaluate predicate
    if (whileLoop.predicateValue == "") {
      available.clear()
      available ++= inputs
      out.append(";{\n")
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, out, available, new ArrayBuffer[DeliteOP]) //dummy syncList b/c already added
      out.append("pred = ") //update var
      out.append(getSym(whileLoop.predicateGraph.result._1, whileLoop.predicateGraph.result._2))
      out.append("\n}\n")
    }

    //print end of while and method
    out.append("}\n}\n")

    //the sync methods/objects
    addSync(syncList, out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString, kernelName)
  }

  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)

  protected def executableName = "While_" + baseId + "_"

}

class CudaGPUWhileGenerator(whileLoop: OP_While, location: Int) extends GPUWhileGenerator(whileLoop, location, Targets.Cuda) with CudaGPUExecutableGenerator {
  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    CudaMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaWhileGenerator(whileLoop, location, target).emitScala(syncList), kernelName)
  }
}

class OpenCLGPUWhileGenerator(whileLoop: OP_While, location: Int) extends GPUWhileGenerator(whileLoop, location, Targets.OpenCL) with OpenCLGPUExecutableGenerator {
  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    OpenCLMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaWhileGenerator(whileLoop, location, target).emitScala(syncList), kernelName)
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
