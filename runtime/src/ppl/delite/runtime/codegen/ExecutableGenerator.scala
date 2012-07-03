package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler.{OpList, PartialSchedule}
import ppl.delite.runtime.Config
import sync.ScalaSyncGenerator

/**
 * Author: Kevin J. Brown
 * Date: Oct 26, 2010
 * Time: 8:19:19 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Generates optimized DeliteExecutable for a given schedule
 * This generator creates a single executable function for each resource based on the provided schedule
 * The resulting executable should have minimal indirection and overhead, as well as increase compiler optimization opportunities
 *
 * This generator makes the following synchronization optimizations:
 * 1) it generates a synchronized getter for dependencies from other resources for the first use only
 *    outputs created in the current resource or already retrieved from another resource are accessed through local variables
 * 2) it generates synchronized result publication only for outputs that other resources will need to consume
 *    outputs that the scheduler has restricted to this resource are kept local 
 */

trait ExecutableGenerator {

  protected val out: StringBuilder = new StringBuilder
  val location: Int
  val kernelPath: String

  def makeExecutable(ops: OpList) {
    writeHeader()
    writeMethodHeader()
    addKernelCalls(ops)
    writeMethodFooter()
    writeFooter()

    addSource(out.toString)
  }

  protected def addKernelCalls(resource: OpList) {
    for (op <- resource) {
      if (op.isInstanceOf[OP_Nested] || op.isInstanceOf[Sync]) //runtime responsible for implementation
        makeNestedFunction(op)
      writeFunctionCall(op)
    }
  }

  def executableName: String = executableName(location)
  def executableName(location: Int): String

  protected def addSource(source: String)

  protected def makeNestedFunction(op: DeliteOP)
  protected def writeFunctionCall(op: DeliteOP)

  protected def writeHeader()
  protected def writeMethodHeader()

  protected def writeFooter()
  protected def writeMethodFooter()

}

trait ScalaExecutableGenerator extends ExecutableGenerator {

  protected def addSource(source: String) {
    ScalaCompile.addSource(source, executableName)
  }

  protected def writeHeader() {
    out.append("import ppl.delite.runtime.codegen.DeliteExecutable\n") //base trait
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    ScalaExecutableGenerator.writePath(kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(executableName)
    out.append(" extends DeliteExecutable {\n")
  }

  protected def writeMethodHeader() {
    out.append("def run() {\n")
  }

  protected def writeMethodFooter() {
    out.append("}\n")
  }

  protected def writeFooter() {
    addAccessor() //provides a reference to the object instance
    out.append("}\n") //end object
  }

  //TODO: can/should this be factored out? need some kind of factory for each target
  //TODO: why is multiloop codegen handled differently?
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => new ScalaConditionGenerator(c, location, kernelPath).makeExecutable()
    case w: OP_While => new ScalaWhileGenerator(w, location, kernelPath).makeExecutable()
    case v: OP_Variant => new ScalaVariantGenerator(v, location, kernelPath).makeExecutable()
    case err => sys.error("Unrecognized OP type: " + err.getClass.getSimpleName)
  }

  protected def writeFunctionCall(op: DeliteOP) {
    def returnsResult = op.outputType(op.getOutputs.head) == op.outputType
    def resultName = if (returnsResult) getSym(op, op.getOutputs.head) else "op_" + getSym(op, op.id)

    if (op.task == null) return //dummy op
    if (Config.profile && !op.isInstanceOf[OP_MultiLoop])
      out.append("PerformanceTimer.start(\""+op.id+"\", Thread.currentThread.getName(), false)\n")
    out.append("val ")
    out.append(resultName)
    out.append(" : ")
    out.append(op.outputType)
    out.append(" = ")
    out.append(op.task)
    out.append('(')
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(',') //no comma before first argument
      first = false
      out.append(getSym(input, name))
    }
    out.append(")\n")
    if (Config.profile && !op.isInstanceOf[OP_MultiLoop])
      out.append("PerformanceTimer.stop(\""+op.id+"\", false)\n")

    if (!returnsResult) {
      for (name <- op.getOutputs) {
        out.append("val ")
        out.append(getSym(op, name))
        out.append(" : ")
        out.append(op.outputType(name))
        out.append(" = ")
        out.append(resultName)
        out.append('.')
        out.append(name)
        out.append('\n')
      }
    }
  }

  protected def getSym(op: DeliteOP, name: String): String = {
    "x"+name
  }

  protected def addAccessor() {
    out.append("def self = this\n")
  }

}

class ScalaMainExecutableGenerator(val location: Int, val kernelPath: String)
  extends ScalaExecutableGenerator with ScalaSyncGenerator {

  def executableName(location: Int) = "Executable" + location
}

object ScalaExecutableGenerator {

  def makeExecutables(schedule: PartialSchedule, kernelPath: String) {
    for (i <- 0 until schedule.numResources) {
      new ScalaMainExecutableGenerator(i, kernelPath).makeExecutable(schedule(i))
    }
  }

  private[codegen] def writePath(kernelPath: String, out: StringBuilder) {
    if (kernelPath == "") return
    out.append("import generated.scala._\n")
    /*
    var begin = 0
    var end = kernelPath.length
    if (kernelPath.startsWith("/")) begin += 1
    if (kernelPath.endsWith("/")) end -= 1
    val packageName = kernelPath.replace('/','.').substring(begin,end)
    out.append(packageName)
    out.append(".scala._\n")
    */
  }
}
