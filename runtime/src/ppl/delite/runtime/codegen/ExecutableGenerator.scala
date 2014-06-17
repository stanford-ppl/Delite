package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.scheduler.OpList
import collection.mutable.ArrayBuffer
import sync.SyncObjectGenerator

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
  val graph: DeliteTaskGraph
  var opList: OpList = _

  def makeExecutable(ops: OpList) {
    opList = ops
    writeHeader()
    writeMethodHeader()
    addKernelCalls(ops)
    writeMethodFooter()
    writeFooter()
    writeSyncObject()

    addSource(out.toString)
  }

  protected def addKernelCalls(resource: OpList) {
    initializeBlock()
    for (op <- resource) {
      if (op.isInstanceOf[OP_Nested] || op.isInstanceOf[Sync] || op.isInstanceOf[PCM_M]) //runtime responsible for implementation
        makeNestedFunction(op)
      writeFunctionCall(op)
    }
    finalizeBlock()
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

  protected def writeSyncObject()

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value): SyncObjectGenerator
  protected def getOpSym(op: DeliteOP) = getSym(op, "op_"+op.id)
  protected def getSym(op: DeliteOP, name: String): String = {
    "x"+name
  }

  protected def initializeBlock() { }
  protected def finalizeBlock() { }
}
