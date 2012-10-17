package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.ops.{Send, DeliteOP, OP_Variant}
import ppl.delite.runtime.codegen.hosts.Hosts
import sync.{CppSyncGenerator, CppSyncObjectGenerator, ScalaSyncObjectGenerator, ScalaSyncGenerator}

/**
 * Author: Kevin J. Brown
 * Date: 1/21/11
 * Time: 3:55 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait VariantGenerator extends NestedGenerator {

  val variant: OP_Variant
  val nested = variant

  def makeExecutable() {
    val hasOutput = variant.outputType != "Unit"

    updateOP()
    //header
    writeHeader()
    writeMethodHeader()

    //output body
    addKernelCalls(variant.variantGraph.schedule(location))
    if (hasOutput) {
      writeOutput(variant.variantGraph.result._1, variant.variantGraph.result._2)
    }

    writeMethodFooter()
    writeFooter()

    writeSyncObject()

    addSource(out.toString)
  }

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], host: Hosts.Value) = {
    host match {
      case Hosts.Scala => new ScalaVariantGenerator(variant, location, kernelPath) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Hosts.Cpp => new CppVariantGenerator(variant, location, kernelPath) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + host.toString)
    }
  }
}

class ScalaVariantGenerator(val variant: OP_Variant, val location: Int, val kernelPath: String)
  extends VariantGenerator with ScalaNestedGenerator with ScalaSyncGenerator {
  def executableName(location: Int) = "Variant_" + baseId + "_" + location

  override protected def getSym(op: DeliteOP, name: String) = VariantCommon.getSym(baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = VariantCommon.getSync(baseId, op, name)
}

class CppVariantGenerator(val variant: OP_Variant, val location: Int, val kernelPath: String)
  extends VariantGenerator with CppNestedGenerator with CppSyncGenerator {
  def executableName(location: Int) = "Variant_" + baseId + "_" + location

  override protected def getSym(op: DeliteOP, name: String) = VariantCommon.getSym(baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = VariantCommon.getSync(baseId, op, name)
}

private [codegen] object VariantCommon { //TODO: traits?
  def getSym(baseId: String, op: DeliteOP, name: String) = "x" + baseId + "_" + name
  def getSync(baseId: String, op: DeliteOP, name: String) = "Result_" + baseId + "_" + name
}
