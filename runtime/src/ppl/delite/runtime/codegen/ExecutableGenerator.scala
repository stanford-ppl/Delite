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

  // CudaExecutableGenerator refers CppExecutableGenerator (for methods writeFunctionCall and writeHeader)
  // that emits to the same StringBuilder (Better way? Using inheritance makes the generator hierarchy somewhat weird)
  var out: StringBuilder = new StringBuilder
  val location: Int
  val graph: DeliteTaskGraph
  var opList: OpList = _

  /**
   * [COMMENT TODO] What does this method do?
   * Entry point for all code generators. Generates a wrapper function whose body
   * is a list of function calls for a list of ops. Code generation in the runtime
   * mostly involves generating method calls from a schedule for each
   * resource. This is split into the following steps:
   * 1. writeHeader: Generate header that goes before the wrapper function
   * 2. writeMethodHeader: Generate the wrapper function signature
   * 3. addKernelCalls: Generate list of function calls that make up the wrapper function's body.
   *    Depending on the op, one of two methods are used to generate function call:
   *    - makeNestedFunction: For [[OP_Nested]], [[Sync]], [[PCM_M]] nodes
   *    - writeFunctionCall: For all other nodes
   * 4. Generate wrapper function's footer
   * 5. Generate footer that goes after the wrapper function
   * 6. Generate all sync objects (<-- what are these?)
   * Once generation is done, the generated code is added as a source file using the
   * 'addSource' method
   * @param ops: List of ops ([[OpList]]) scheduled on one resource to be code-generated
   */
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

  /**
   * [COMMENT TODO] What does this method do?
   * Generate list of function calls that make up the wrapper function's body.
   * Depending on the op, one of two methods are used to generate function call:
   *  - makeNestedFunction: For [[OP_Nested]], [[Sync]], [[PCM_M]] nodes
   *  - writeFunctionCall: For all other nodes
   * Individual code generators must override the above two methods as required
   * @param resource: List of ops [[OpList]] to be code-generated
   */
  protected def addKernelCalls(resource: OpList) {
    initializeBlock()
    for (op <- resource) {
      if (op.isInstanceOf[OP_Nested] || op.isInstanceOf[Sync] || op.isInstanceOf[PCM_M]) //runtime responsible for implementation
        makeNestedFunction(op)
      writeFunctionCall(op)
    }
    finalizeBlock()
  }

  /** Name of the wrapper function */
  def executableName: String = executableName(location)
  def executableName(location: Int): String

  /**
   * Adds generated code  as a separate file
   * for compilation. Typically overridden in individual code
   * generators to add a new file to their companion 'Compile'
   * object
   * @param source: Generated code in String format
   */
  protected def addSource(source: String)

  /**
   * Generate function call for certain kinds of [[DeliteOP]]s.
   * At the time of this writing, this method is used for
   * [[OP_Nested]], [[Sync]], and [[PCM_M]] nodes
   * @param op: DeliteOP to be generated
   */
  protected def makeNestedFunction(op: DeliteOP)

  /**
   * Generate function call for all [[DeliteOP]]s not generated
   * using makeNestedFunction
   * @param op: DeliteOP to be generated
   */
  protected[codegen] def writeFunctionCall(op: DeliteOP)

  /**
   * Write header that goes before wrapper function
   * Overridden by individual code generators to typically
   * include/import files and libraries, declare globals, etc
   */
  protected[codegen] def writeHeader()

  /**
   * Write wrapper function signature
   */
  protected def writeMethodHeader()

  /**
   * Write footer that goes after wrapper function
   */
  protected def writeFooter()

  /**
   * Write wrapper function footer. Typically involves
   * generating frees, closing braces
   */
  protected def writeMethodFooter()

  /**
   * [COMMENT TODO] What does this method do?
   */
  protected[codegen] def writeSyncObject()

  /**
   * [COMMENT TODO] What do the following methods do?
   */
  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value): SyncObjectGenerator
  protected def getOpSym(op: DeliteOP) = getSym(op, "op_"+op.id)
  protected def getSym(op: DeliteOP, name: String): String = {
    "x"+name
  }

  /**
   * Generates code that goes before the list of function calls
   * in the wrapper function's body
   */
  protected def initializeBlock() { }

  /**
   * Generates code that goes after the list of function calls
   * in the wrapper function's body
   */
  protected def finalizeBlock() { }
}
