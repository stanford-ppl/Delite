package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.{DeliteOP, OP_Condition}
import collection.mutable.ArrayBuffer

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:33 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class ConditionGenerator(condition: OP_Condition, location: Int) extends ExecutableGenerator {

  val baseId = condition.id.slice(0, condition.id.indexOf('_'))

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val hasOutput = condition.outputType != "Unit"
    val inputs = (condition.predicateGraph.inputs ++ condition.thenGraph.inputs ++ condition.elseGraph.inputs)

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    //output predicate
    if (condition.predicateValue == "") {
      available ++= inputs
      addKernelCalls(condition.predicateGraph.schedule(location), location, out, available, syncList)
    }

    //write if
    out.append("if (")
    if (condition.predicateValue == "") out.append(getSym(condition.predicateGraph.result))
    else out.append(condition.predicateValue)
    out.append(") {\n")

    //output if body
    if (condition.thenValue == "") {
      available.clear
      available ++= inputs
      addKernelCalls(condition.thenGraph.schedule(location), location, out, available, syncList)
      if (hasOutput) out.append(getSym(condition.thenGraph.result))
    }
    else if (hasOutput) out.append(condition.thenValue)
    if (hasOutput) out.append('\n')

    //print else
    out.append("} else {\n")

    //output else body
    if (condition.elseValue == "") {
      available.clear
      available ++= inputs
      addKernelCalls(condition.elseGraph.schedule(location), location, out, available, syncList)
      if (hasOutput) out.append(getSym(condition.elseGraph.result))
    }
    else if (hasOutput) out.append(condition.elseValue)
    if (hasOutput) out.append('\n')

    //print end of if and method
    out.append("}\n}\n")

    //the sync methods/objects
    addSync(syncList, out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString)
  }

  private def updateOP() {
    condition.setExecutableName(kernelName)
  }

  protected def executableName = "Condition_" + baseId + "_"

  private def kernelName = executableName + location

  override def getSync(op: DeliteOP) = "Result_" + baseId + "_" + op.id

  private def writeHeader(location: Int, out: StringBuilder) {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(condition.predicateGraph.kernelPath, out) //package of scala kernels
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }

  private def writeMethodHeader(out: StringBuilder) {
    out.append("def apply(")
    writeInputs(out)
    out.append("): ")
    out.append(condition.outputType)
    out.append(" = {\n")
  }

  private def writeInputs(out: StringBuilder) {
    var first = true
    for (in <- condition.getNestedInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(in))
      out.append(": ")
      out.append(in.outputType)
    }
  }

}
