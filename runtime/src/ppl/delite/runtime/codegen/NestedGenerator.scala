package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.DeliteTaskGraph
import collection.mutable.ArrayBuffer
import java.lang.annotation.Target
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets
import sync._

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
    ScalaExecutableGenerator.writePackage(graph, out)
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import ppl.delite.runtime.profiler.MemoryProfiler\n")
    ScalaExecutableGenerator.writePath(graph, out) //package of scala kernels

    val locationsRecv = nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Receive])).map(_.asInstanceOf[Receive].sender.from.scheduledResource).toSet
    val locations = if (nested.nestedGraphs.flatMap(_.schedule(location).toArray.filter(_.isInstanceOf[Send])).nonEmpty) Set(location) union locationsRecv
                    else locationsRecv
    if (!this.isInstanceOf[SyncObjectGenerator]) writeJNIInitializer(locations)

    out.append("object ")
    out.append(executableName)
    out.append(" {\n")
    if (Config.profile) out.append("val threadName = Thread.currentThread.getName()\n")
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

trait CppNestedGenerator extends NestedGenerator with CppExecutableGenerator with CppResourceInfoGenerator {

  private val target = Targets.Cpp

  def generateMethodSignature(): String = {
    val str = new StringBuilder
    str.append("#include \"" + target + "helperFuncs.h\"\n")
    str.append(nested.outputType(target))
    str.append(' ')
    if (!isPrimitiveType(nested.outputType) && nested.outputType!="Unit" && Config.cppMemMgr!="refcnt") str.append(" *")
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
    str.append(resourceInfoType)
    str.append(" &")
    str.append(resourceInfoSym)
    for ((op,sym) <- inputs) {
      str.append(", ")
      str.append(op.outputType(target,sym))
      str.append(addRef(op.outputType(sym)))
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


trait CudaNestedGenerator extends NestedGenerator with CudaExecutableGenerator with SyncGenerator {

  // referential primitive is a primitive type result of a CUDA kernel stored on the device (e.g. reduction)
  def isReferentialPrimitive(op: DeliteOP, sym: String): Boolean = {
    if(isPrimitiveType(op.outputType(sym))) {
      op match {
        case n:OP_Nested => false // referentialPrimitive is returned as a normal primitive type from nested OPs (converted internally)
        case i:OP_Input if(i.op.isInstanceOf[OP_Nested]) => false
        case i:OP_Input if(i.op.scheduledResource == location) => true
        case _ if(op.scheduledResource == location) => true
        case _ => false
      }
    }
    else 
      false 
  }

  def generateMethodSignature(): String = {
    val str = new StringBuilder
    str.append(nested.outputType(deviceTarget))
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
      str.append(op.outputType(deviceTarget, sym))
      if (!isPrimitiveType(op.outputType(sym)) || isReferentialPrimitive(op,sym)) str.append(" *")
      str.append(' ')
      str.append(getSymDevice(op, sym))
      if (updateOps(nested).contains(sym)) {
        str.append(',')
        str.append(op.outputType(hostTarget,sym))
        if (!isPrimitiveType(op.outputType(sym))) str.append(" *")
        str.append(' ')
        str.append(getSymHost(op, sym))
        str.append(',')
        str.append(getJNIType(op.outputType(Targets.Scala, sym))) //FIXME: Use remote target
        str.append(' ')
        str.append(getSymRemote(op, sym)) 
      }
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
    val devType = op.outputType(Targets.Cuda, name)
    //TODO: put this into cuda sync generator
    //TODO: make sure symbol is not freed before recvCuda is called
    if (isReferentialPrimitive(op,name)) 
      out.append("recvCuda_%s(%s)".format(mangledName(devType),getSymDevice(op,name)))
    else 
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

