package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import java.lang.annotation.Target
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets
import sync.SyncObjectGenerator

/**
 * Author: Kevin J. Brown
 * Date: 1/23/11
 * Time: 2:31 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait NestedGenerator extends ExecutableGenerator {

  val nested: OP_Nested

  protected def baseId = nested.id.slice(0, nested.id.indexOf('_'))

  protected def updateOP() {
    nested.setExecutableName(executableName)
  }

  def makeExecutable()

  protected def writeMethodHeader()

  protected def writeInputs(inputs: Seq[(DeliteOP,String)] = nested.getInputs)

  protected def writeReturn(newLine: Boolean = true)
  protected def writeOutput(op: DeliteOP, name: String, newLine: Boolean = true)
  protected def writeValue(value: String, newLine: Boolean = true)

}

trait ScalaNestedGenerator extends NestedGenerator with ScalaExecutableGenerator {

  override protected def writeHeader() {
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    ScalaExecutableGenerator.writePath(kernelPath, out) //package of scala kernels

    val locationsRecv = nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Receive])).map(_.asInstanceOf[Receive].sender.from.scheduledResource).toSet
    val locations = if (nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Send])).nonEmpty) Set(location) union locationsRecv
                    else locationsRecv
    if (!this.isInstanceOf[SyncObjectGenerator]) writeJNIInitializer(locations)

    out.append("object ")
    out.append(executableName)
    out.append(" {\n")
  }

  override protected def writeMethodHeader() {
    out.append("def apply(")
    writeInputs()
    out.append("): ")
    out.append(nested.outputType)
    out.append(" = {\n")
  }

  override protected def writeFooter() {
    out.append("}\n")
  }

  protected def writeInputs(inputs: Seq[(DeliteOP,String)] = nested.getInputs) {
    var first = true
    for ((op,sym) <- inputs) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(op, sym))
      out.append(": ")
      out.append(op.outputType(sym))
    }
  }

  protected def writeReturn(newLine: Boolean = true) { }

  protected def writeOutput(op: DeliteOP, name: String, newLine: Boolean = true) {
    out.append(getSym(op, name))
    if (newLine) out.append('\n')
  }

  protected def writeValue(value: String, newLine: Boolean = true) {
    out.append(value)
    if (newLine) out.append('\n')
  }

}


trait CppNestedGenerator extends NestedGenerator with CppExecutableGenerator {

  def generateMethodSignature(): String = {
    val str = new StringBuilder
    str.append("#include \"cppHeader.hpp\"\n")
    str.append(nested.outputType(Targets.Cpp))
    str.append(' ')
    if (!isPrimitiveType(nested.outputType) && nested.outputType!="Unit") out.append(" *")
    str.append(executableName)
    str.append('(')
    str.append(generateInputs())
    str.append(")")
    str.toString
  }

  override protected def writeMethodHeader() {
    out.append(generateMethodSignature)
    out.append(" {\n")
    // Add references to other executables for sync objects
    val locationsRecv = nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Receive])).map(_.asInstanceOf[Receive].sender.from.scheduledResource).toSet
    val locations = if (nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Send])).nonEmpty) Set(location) union locationsRecv
                    else locationsRecv
    writeJNIInitializer(locations)
  }

  override protected def writeMethodFooter() {
    out.append("}\n")
  }

  protected def generateInputs(inputs: Seq[(DeliteOP,String)] = nested.getInputs): String = {
    val str = new StringBuilder
    var first = true
    for ((op,sym) <- inputs) {
      if (!first) str.append(", ")
      first = false
      str.append(CppExecutableGenerator.typesMap(Targets.Cpp)(sym))
      if (!isPrimitiveType(op.outputType(sym))) str.append(" *")
      str.append(' ')
      str.append(getSymHost(op, sym))
    }
    str.toString
  }

  protected def writeInputs(inputs: Seq[(DeliteOP,String)] = nested.getInputs) {
    out.append(generateInputs(inputs))
  }

  protected def writeReturn(newLine: Boolean = true) {
    if (newLine) out.append("return;\n")
    else out.append("return ")
  }

  protected def writeOutput(op: DeliteOP, name: String, newLine: Boolean = true) {
    out.append(getSymHost(op, name))
    if (newLine) out.append(";\n")
  }

  protected def writeValue(value: String, newLine: Boolean = true) {
    out.append(value)
    if (newLine) out.append(";\n")
  }

}


/*
abstract class GPUNestedGenerator(nested: OP_Nested, location: Int, target: Targets.Value) extends GPUExecutableGenerator {

  protected val baseId = nested.id.slice(0, nested.id.indexOf('_'))

  protected def updateOP() {
    nested.setExecutableName(executableName)
  }

  protected def executableName = executableName + location

  override protected def getScalaSym(op: DeliteOP, name: String) = NestedCommon.getSym(baseId, op, name)

  protected def writeFunctionHeader(out: StringBuilder) {
    out.append(nested.outputType(target))
    out.append(' ')
    // GPU nested block can only return when both condition branches are returned by GPU,
    // meaning that the return object will be a pointer type
    if(nested.outputType != "Unit") out.append('*')
    out.append(executableName)
    out.append('(')
    writeInputs(out)
    out.append(") {\n")
  }

  protected def writeInputs(out: StringBuilder) {
    var first = true

    val metadata = nested.getGPUMetadata(target)

    for ((in, sym) <- nested.getInputs) {
      if (!first) out.append(',')
      first = false
      out.append(resultType(in,sym))
      out.append(ref(in,sym))
      out.append(' ')
      out.append(getSymGPU(sym))
      if ((nested.getMutableInputs contains (in,sym)) && (in.scheduledResource != nested.scheduledResource)) {
        out.append(',')
        out.append(getJNIType(in.outputType(sym)))
        out.append(' ')
        out.append(getSymCPU(sym))
      }
    }

    def resultType(op: DeliteOP, sym: String) = {
      if (metadata.inputs.contains(op,sym))
        metadata.inputs(op,sym).resultType
      else if (isPrimitiveType(op.outputType(sym)))
        getCPrimitiveType(op.outputType(sym))
      else
        error("op " + op.id + " with return type " + op.outputType + " does not have a known C type")
    }

    def ref(op: DeliteOP, sym: String) = {
      if (isPrimitiveType(op.outputType(sym))) "&"
      else "*"
      //if (op.supportsTarget(target)) "*"
      //else "&"
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

  protected def executableName = executableName + location

  override protected def getSym(op: DeliteOP, name: String) = NestedCommon.getSym(baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = NestedCommon.getSync(baseId, op, name)

  protected def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ScalaExecutableGenerator.writePath(nested.nestedGraphs(0).kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(executableName)
    out.append(" {\n")
  }
}*/

