package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_Variant}

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
