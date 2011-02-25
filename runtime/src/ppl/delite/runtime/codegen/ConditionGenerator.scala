package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.ops.{OP_Input, DeliteOP, OP_Condition}

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:33 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class ConditionGenerator(condition: OP_Condition, location: Int) extends NestedGenerator(condition, location) {

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val hasOutput = condition.outputType != "Unit"

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    //output predicate
    if (condition.predicateValue == "") {
      available += OP_Input
      addKernelCalls(condition.predicateGraph.schedule(location), location, out, available, syncList)
    }

    //write if
    out.append("if (")
    if (condition.predicateValue == "") out.append(getSym(condition.predicateGraph.result._2))
    else out.append(condition.predicateValue)
    out.append(") {\n")

    //output if body
    if (condition.thenValue == "") {
      available.clear
      available += OP_Input
      addKernelCalls(condition.thenGraph.schedule(location), location, out, available, syncList)
      if (hasOutput) out.append(getSym(condition.thenGraph.result._2))
    }
    else if (hasOutput) out.append(condition.thenValue)
    if (hasOutput) out.append('\n')

    //print else
    out.append("} else {\n")

    //output else body
    if (condition.elseValue == "") {
      available.clear
      available += OP_Input
      addKernelCalls(condition.elseGraph.schedule(location), location, out, available, syncList)
      if (hasOutput) out.append(getSym(condition.elseGraph.result._2))
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

  protected def executableName = "Condition_" + baseId + "_"

}

class GPUConditionGenerator(condition: OP_Condition, location: Int) extends GPUNestedGenerator(condition, location) {

  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    GPUMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaConditionGenerator(condition, location).emitScala(syncList))
  }

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val hasOutput = condition.outputType != "Unit"
    assert(hasOutput == false) //TODO: we can relax this by conditionally selecting the proper metadata functions as well

    writeFunctionHeader(out)
    writeJNIInitializer(location, out)

    val available = new ArrayBuffer[DeliteOP]
    val awaited = new ArrayBuffer[DeliteOP]

    //output predicate
    if (condition.predicateValue == "") {
      available += OP_Input
      awaited += OP_Input
      addKernelCalls(condition.predicateGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //write if
    out.append("if (")
    if (condition.predicateValue == "") out.append(getSymGPU(condition.predicateGraph.result._2))
    else out.append(condition.predicateValue)
    out.append(") {\n")

    //output if body
    if (condition.thenValue == "") {
      available.clear
      available += OP_Input
      awaited.clear
      awaited += OP_Input
      addKernelCalls(condition.thenGraph.schedule(location), location, available, awaited, syncList, out)
      if (hasOutput) {
        out.append("return ")
        out.append(getSymGPU(condition.thenGraph.result._2))
      }
    }
    else if (hasOutput) {
      out.append("return ")
      out.append(condition.thenValue)
    }
    if (hasOutput) out.append(";\n")

    //print else
    out.append("} else {\n")

    //output else body
    if (condition.elseValue == "") {
      available.clear
      available += OP_Input
      awaited.clear
      awaited += OP_Input
      addKernelCalls(condition.elseGraph.schedule(location), location, available, awaited, syncList, out)
      if (hasOutput) {
        out.append("return ")
        out.append(getSymGPU(condition.elseGraph.result._2))
      }
    }
    else if (hasOutput) {
      out.append("return ")
      out.append(condition.elseValue)
    }
    if (hasOutput) out.append(";\n")

    //print end of if and function
    out.append("}\n}\n")

    out.toString
  }

  protected def executableName = "Condition_" + baseId + "_"

}

class GPUScalaConditionGenerator(condition: OP_Condition, location: Int) extends GPUScalaNestedGenerator(condition, location) {
  protected def executableName = "Condition_" + baseId + "_"
}
        