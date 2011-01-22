package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_Variant}
import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/21/11
 * Time: 3:55 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class VariantGenerator(variant: OP_Variant, location: Int) extends ExecutableGenerator {

  val baseId = variant.id.slice(0, variant.id.indexOf('_'))

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val hasOutput = variant.outputType != "Unit"
    val inputs = variant.variantGraph.inputs

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    available ++= inputs

    //output body
    addKernelCalls(variant.variantGraph.schedule(location), location, out, available, syncList)
    if (hasOutput) {
      out.append(getSym(variant.variantGraph.result))
      out.append('\n')
    }
    out.append("}\n") //end of method

    //the sync methods/objects
    addSync(syncList, out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString)
  }

  private def updateOP() {
    variant.setExecutableName(kernelName)
  }

  protected def executableName = "Variant_" + baseId + "_"

  private def kernelName = executableName + location

  override def getSync(op: DeliteOP) = "Result_" + baseId + "_" + op.id

  private def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(variant.variantGraph.kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }

  private def writeMethodHeader(out: StringBuilder) {
    out.append("def apply(")
    writeInputs(out)
    out.append("): ")
    out.append(variant.outputType)
    out.append(" = {\n")
  }

  private def writeInputs(out: StringBuilder) {
    var first = true
    for (in <- variant.getNestedInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(in))
      out.append(": ")
      out.append(in.outputType)
    }
  }

}

class GPUVariantGenerator(variant: OP_Variant, location: Int) extends GPUExecutableGenerator {

  val baseId = variant.id.slice(0, variant.id.indexOf('_'))

  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    GPUMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaVariantGenerator(variant, location).emitScala(syncList))
  }

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val inputs = variant.variantGraph.inputs
    val hasOutput = variant.outputType != "Unit"

    writeFunctionHeader(out)
    writeJNIInitializer(location, out)

    val available = new ArrayBuffer[DeliteOP]
    val awaited = new ArrayBuffer[DeliteOP]
    available ++= inputs
    awaited ++= inputs

    //output body
    addKernelCalls(variant.variantGraph.schedule(location), location, available, awaited, syncList, out)
    if (hasOutput) {
      out.append("return ")
      out.append(getSymGPU(variant.variantGraph.result))
      out.append(";\n")
    }
    out.append("}\n") //end of function

    out.toString
  }

  private def updateOP() {
    variant.setExecutableName(kernelName)
  }

  protected def executableName = "Variant_" + baseId + "_"

  private def kernelName = executableName + location

  private def writeFunctionHeader(out: StringBuilder) {
    out.append(variant.outputType(Targets.Cuda))
    out.append(' ')
    out.append(kernelName)
    out.append('(')
    writeInputs(out)
    out.append(") {\n")
  }

  private def writeInputs(out: StringBuilder) {
    var first = true
    for (in <- variant.getNestedInputs) {
      if (!first) out.append(", ")
      first = false
      if (variant.cudaMetadata.inputs.contains(in)) {
        out.append(variant.cudaMetadata.inputs(in).resultType)
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

class GPUScalaVariantGenerator(variant: OP_Variant, location: Int) extends GPUScalaExecutableGenerator {

  val baseId = variant.id.slice(0, variant.id.indexOf('_'))

  def emitScala(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder
    writeHeader(location, out)
    addSync(syncList, out) //the sync methods/objects
    writeOuterSet(syncList, out) //helper set methods for JNI calls to access
    out.append("}\n")
    out.toString
  }

  protected def executableName = "Variant_" + baseId + "_"

  private def kernelName = executableName + location

  override protected def getSync(op: DeliteOP) = "Result_" + baseId + "_" + op.id

  private def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(variant.variantGraph.kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }
}

