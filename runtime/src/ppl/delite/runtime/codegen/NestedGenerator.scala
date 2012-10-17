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

  private val target = Targets.Cpp
  private val typesMap = CppExecutableGenerator.typesMap

  def generateMethodSignature(): String = {
    val str = new StringBuilder
    str.append("#include \"cppHeader.hpp\"\n")
    str.append(nested.outputType(target))
    str.append(' ')
    if (!isPrimitiveType(nested.outputType) && nested.outputType!="Unit") str.append(" *")
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
      str.append(typesMap(target)(sym))
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


trait CudaNestedGenerator extends NestedGenerator with CudaExecutableGenerator {

  private val target = Targets.Cuda
  private val typesMap = CudaExecutableGenerator.typesMap

  def generateMethodSignature(): String = {
    val str = new StringBuilder
    str.append(nested.outputType(target))
    str.append(' ')
    if (!isPrimitiveType(nested.outputType) && nested.outputType!="Unit") str.append(" *")
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
      str.append(typesMap(target)(sym))
      if (!isPrimitiveType(op.outputType(sym))) str.append(" *")
      str.append(' ')
      str.append(getSymDevice(op, sym))
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
    out.append(getSymDevice(op, name))
    if (newLine) out.append(";\n")
  }

  protected def writeValue(value: String, newLine: Boolean = true) {
    out.append(value)
    if (newLine) out.append(";\n")
  }

  override protected def initializeBlock() {
    available.clear
  }

}

