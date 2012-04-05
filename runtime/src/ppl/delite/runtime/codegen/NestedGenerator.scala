package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{OP_Nested, DeliteOP}
import ppl.delite.runtime.graph.targets.Targets
import java.lang.annotation.Target

/**
 * Author: Kevin J. Brown
 * Date: 1/23/11
 * Time: 2:31 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class NestedGenerator(nested: OP_Nested, location: Int) extends ExecutableGenerator {

  protected val baseId = nested.id.slice(0, nested.id.indexOf('_'))

  protected def updateOP() {
    nested.setExecutableName(kernelName)
  }

  protected def kernelName = executableName + location

  override protected def getSym(op: DeliteOP, name: String) = NestedCommon.getSym(baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = NestedCommon.getSync(baseId, op, name)

  protected def writeHeader(location: Int, out: StringBuilder) {
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(nested.nestedGraphs(0).kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }

  protected def writeMethodHeader(out: StringBuilder) {
    out.append("def apply(")
    writeInputs(out)
    out.append("): ")
    out.append(nested.outputType)
    out.append(" = {\n")
  }

  protected def writeInputs(out: StringBuilder) {
    var first = true
    for ((op,sym) <- nested.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(op, sym))
      out.append(": ")
      out.append(op.outputType(sym))
    }
  }
}

abstract class GPUNestedGenerator(nested: OP_Nested, location: Int, target: Targets.Value) extends GPUExecutableGenerator {

  protected val baseId = nested.id.slice(0, nested.id.indexOf('_'))

  protected def updateOP() {
    nested.setExecutableName(kernelName)
  }

  protected def kernelName = executableName + location

  override protected def getScalaSym(op: DeliteOP, name: String) = NestedCommon.getSym(baseId, op, name)

  protected def writeFunctionHeader(out: StringBuilder) {
    out.append(nested.outputType(target))
    out.append(' ')
    // GPU nested block can only return when both condition branches are returned by GPU,
    // meaning that the return object will be a pointer type
    if(nested.outputType != "Unit") out.append('*')
    out.append(kernelName)
    out.append('(')
    writeInputs(out)
    out.append(") {\n")
  }

  protected def writeInputs(out: StringBuilder) {
    var first = true

    val metadata = nested.getGPUMetadata(target)

    for ((in, sym) <- nested.getInputs) {
      if (metadata.inputs.contains((in,sym))) {
        if (!first) out.append(',')
        first = false
        out.append(metadata.inputs((in,sym)).resultType)
        out.append("* ")
        out.append(getSymGPU(sym))
        if ((nested.getMutableInputs. contains (in,sym)) && (in.getConsumers.filter(_.scheduledResource!=in.scheduledResource).nonEmpty)) {
          out.append(',')
          out.append(getJNIType(in.outputType(sym)))
          out.append(' ')
          out.append(getSymCPU(sym))
        }
      }
      else if (isPrimitiveType(in.outputType(sym))) {
        if (!first) out.append(',')
        first = false
        out.append(getCPrimitiveType(in.outputType(sym)))
        out.append(' ')
        out.append(getSymGPU(sym))
      }
    }
  }
}

abstract class GPUScalaNestedGenerator(nested: OP_Nested, location: Int, target: Targets.Value) extends GPUScalaExecutableGenerator(target) {

  protected val baseId = nested.id.slice(0, nested.id.indexOf('_'))

  def emitScala(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder
    writeHeader(location, out)
    addSync(syncList, out) //the sync methods/objects
    writeOuterSet(syncList, out) //helper set methods for JNI calls to access
    out.append("}\n")
    out.toString
  }

  protected def kernelName = executableName + location

  override protected def getSym(op: DeliteOP, name: String) = NestedCommon.getSym(baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = NestedCommon.getSync(baseId, op, name)

  protected def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(nested.nestedGraphs(0).kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }
}

private [codegen] object NestedCommon { //TODO: traits?
  def getSym(baseId: String, op: DeliteOP, name: String) = "x" + baseId + "_" + name
  def getSync(baseId: String, op: DeliteOP, name: String) = "Result_" + baseId + "_" + name
}
