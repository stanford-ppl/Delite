package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.graph.ops.OP_While
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 4:09 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class WhileGenerator(whileLoop: OP_While, location: Int) extends ExecutableGenerator {

  val baseId = whileLoop.id.slice(0, whileLoop.id.indexOf('_'))

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val inputs = (whileLoop.predicateGraph.inputs ++ whileLoop.bodyGraph.inputs)

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
      out.append(getSym(whileLoop.predicateGraph.result))
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
      available.clear
      available ++= inputs
      addKernelCalls(whileLoop.bodyGraph.schedule(location), location, out, available, syncList)
    }

    //reevaluate predicate
    if (whileLoop.predicateValue == "") {
      available.clear
      available ++= inputs
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, out, available, new ArrayBuffer[DeliteOP]) //dummy syncList b/c already added
      out.append("pred = ") //update var
      out.append(getSym(whileLoop.predicateGraph.result))
      out.append('\n')
    }

    //print end of while and method
    out.append("}\n}\n")

    //the sync methods/objects
    addSync(syncList, out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString)
  }

  private def updateOP() {
    whileLoop.setExecutableName(kernelName)
  }

  protected def executableName = "While_" + baseId + "_"

  private def kernelName = executableName + location

  override protected def getSync(op: DeliteOP) = "Result_" + baseId + "_" + op.id

  private def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(whileLoop.predicateGraph.kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }

  private def writeMethodHeader(out: StringBuilder) {
    out.append("def apply(")
    writeInputs(out)
    out.append(") {\n")
  }

  private def writeInputs(out: StringBuilder) {
    var first = true
    for (in <- whileLoop.getNestedInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(in))
      out.append(": ")
      out.append(in.outputType)
    }
  }

}

class GPUWhileGenerator(whileLoop: OP_While, location: Int) extends GPUExecutableGenerator {

  val baseId = whileLoop.id.slice(0, whileLoop.id.indexOf('_'))

  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    GPUMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaWhileGenerator(whileLoop, location).emitScala(syncList))
  }

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val inputs = (whileLoop.predicateGraph.inputs ++ whileLoop.bodyGraph.inputs)

    writeFunctionHeader(out)
    writeJNIInitializer(location, out)

    val available = new ArrayBuffer[DeliteOP]
    val awaited = new ArrayBuffer[DeliteOP]
    //output predicate
    if (whileLoop.predicateValue == "") {
      available ++= inputs
      awaited ++= inputs
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //write while
    if (whileLoop.predicateValue == "") {
      out.append("bool pred = ")
      out.append(getSymCPU(whileLoop.predicateGraph.result))
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
      available.clear
      available ++= inputs
      awaited.clear
      awaited ++= inputs
      addKernelCalls(whileLoop.bodyGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //reevaluate predicate
    if (whileLoop.predicateValue == "") {
      available.clear
      available ++= inputs
      awaited.clear
      awaited ++= inputs
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, available, awaited, new ArrayBuffer[DeliteOP], out) //dummy syncList b/c already added
      out.append("pred = ") //update var
      out.append(getSymCPU(whileLoop.predicateGraph.result))
      out.append(";\n")
    }

    //print end of while and function
    out.append("}\n}\n")
    out.toString
  }

  private def updateOP() {
    whileLoop.setExecutableName(kernelName)
  }

  protected def executableName = "While_" + baseId + "_"

  private def kernelName = executableName + location

  private def writeFunctionHeader(out: StringBuilder) {
    out.append("void ")
    out.append(kernelName)
    out.append('(')
    writeInputs(out)
    out.append(") {\n")
  }

  private def writeInputs(out: StringBuilder) {
    var first = true
    for (in <- whileLoop.getNestedInputs) {
      if (!first) out.append(", ")
      first = false
      if (whileLoop.cudaMetadata.inputs.contains(in)) {
        out.append(whileLoop.cudaMetadata.inputs(in).resultType)
        out.append(' ')
        out.append(getSymGPU(in))
      }
      else if (getJNIType(in.outputType) != "jobject") {
        out.append(getCPrimitiveType(in.outputType))
        out.append(' ')
        out.append(getSymGPU(in))
      }
      else {
        out.append(getJNIType(in.outputType))
        out.append(' ')
        out.append(getSymCPU(in))
      }
    }
  }

}

class GPUScalaWhileGenerator(whileLoop: OP_While, location: Int) extends GPUScalaExecutableGenerator {

  val baseId = whileLoop.id.slice(0, whileLoop.id.indexOf('_'))

  def emitScala(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder
    writeHeader(location, out)
    addSync(syncList, out) //the sync methods/objects
    writeOuterSet(syncList, out) //helper set methods for JNI calls to access
    out.append("}\n")
    out.toString
  }

  protected def executableName = "While_" + baseId + "_"

  private def kernelName = executableName + location

  override protected def getSync(op: DeliteOP) = "Result_" + baseId + "_" + op.id

  private def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(whileLoop.predicateGraph.kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }
}
