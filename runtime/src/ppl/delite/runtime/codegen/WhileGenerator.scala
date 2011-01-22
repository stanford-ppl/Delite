package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.graph.ops.OP_While
import collection.mutable.ArrayBuffer

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

  override def getSync(op: DeliteOP) = "Result_" + baseId + "_" + op.id

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
