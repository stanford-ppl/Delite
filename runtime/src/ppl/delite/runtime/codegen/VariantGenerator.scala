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

class VariantGenerator(variant: OP_Variant, location: Int) extends NestedGenerator(variant, location) {

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val hasOutput = variant.outputType != "Unit"
    val inputs = variant.variantGraph.inputOps

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    available ++= inputs

    //output body
    addKernelCalls(variant.variantGraph.schedule(location), location, out, available, syncList)
    if (hasOutput) {
      out.append(getSym(variant.variantGraph.result._1, variant.variantGraph.result._2))
      out.append('\n')
    }
    out.append("}\n") //end of method

    //the sync methods/objects
    addSync(syncList, out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString, kernelName)
  }

  protected def executableName = "Variant_" + baseId + "_"

}

class CudaGPUVariantGenerator(variant: OP_Variant, location: Int) extends GPUVariantGenerator(variant, location, Targets.Cuda) with CudaGPUExecutableGenerator {
  def makeExecutable() {
    assert(false, "OP_Variant is temporarily disabled")
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    CudaMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaVariantGenerator(variant, location, target).emitScala(syncList), kernelName)
  }
}
class OpenCLGPUVariantGenerator(variant: OP_Variant, location: Int) extends GPUVariantGenerator(variant, location, Targets.OpenCL) with OpenCLGPUExecutableGenerator {
  def makeExecutable() {
    assert(false, "OP_Variant is temporarily disabled")
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    OpenCLMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaVariantGenerator(variant, location, target).emitScala(syncList), kernelName)
  }
}

abstract class GPUVariantGenerator(variant: OP_Variant, location: Int, target: Targets.Value) extends GPUNestedGenerator(variant, location, target) {

  def makeExecutable(): Unit

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val hasOutput = variant.outputType != "Unit"
    val inputOps = variant.variantGraph.inputOps
    val inputs = variant.variantGraph.inputs
    implicit val aliases = new AliasTable[(DeliteOP,String)]

    writeFunctionHeader(out)
    val locations = variant.nestedGraphs.flatMap(_.ops.map(_.scheduledResource)).toSet union Set(location)
    writeJNIInitializer(locations, out)

    val available = new ArrayBuffer[(DeliteOP,String)]
    val awaited = new ArrayBuffer[DeliteOP]
    available ++= inputs
    awaited ++= inputOps

    //output body
    addKernelCalls(variant.variantGraph.schedule(location), location, available, awaited, syncList, out)
    if (hasOutput) {
      out.append("return ")
      out.append(getSymGPU(variant.variantGraph.result._2))
      out.append(";\n")
    }

    writeJNIFinalizer(locations, out)
    out.append("}\n") //end of function

    out.toString
  }

  protected def executableName = "Variant_" + baseId + "_"

}

class GPUScalaVariantGenerator(variant: OP_Variant, location: Int, target: Targets.Value) extends GPUScalaNestedGenerator(variant, location, target) {
  override protected def executableName = "Variant_" + baseId + "_"
}
