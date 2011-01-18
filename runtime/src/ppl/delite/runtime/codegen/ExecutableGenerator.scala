package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler.PartialSchedule
import java.util.{ArrayDeque, ArrayList}
import collection.mutable.HashSet

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

object ExecutableGenerator {

  def makeExecutables(schedule: PartialSchedule, kernelPath: String) {
    for (i <- 0 until schedule.resources.length) {
      ScalaCompile.addSource(makeExecutable(schedule.resources(i), i, kernelPath))
    }
  }

  private def makeExecutable(resource: ArrayDeque[DeliteOP], location: Int, kernelPath: String) = {
    val out = new StringBuilder //the output string
    val syncList = new ArrayList[DeliteOP] //list of ops needing sync added

    //the header
    writeHeader(out, location, kernelPath)

    //the run method
    out.append("def run() {\n")
    addKernelCalls(resource, location, out, syncList)
    out.append('}')
    out.append('\n')

    //the sync methods/objects
    addSync(syncList, out)

    //an accessor method for the object
    addAccessor(out)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private[codegen] def writeHeader(out: StringBuilder, location: Int, kernelPath: String) {
    out.append("import ppl.delite.runtime.codegen.DeliteExecutable\n") //base trait
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    writePath(kernelPath, out) //package of scala kernels
    out.append("object Executable")
    out.append(location)
    out.append(" extends DeliteExecutable {\n")
  }

  private[codegen] def writePath(kernelPath: String, out: StringBuilder) {
    if (kernelPath == "") return
    out.append("import ")    
    out.append("generated.scala._\n")
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

  private def addKernelCalls(resource: ArrayDeque[DeliteOP], location: Int, out: StringBuilder, syncList: ArrayList[DeliteOP]) {
    val available = new ArrayList[DeliteOP] //ops that have existing local symbols
    val iter = resource.iterator
    while (iter.hasNext) { //foreach op
      val op = iter.next
      //add to available list
      available.add(op)

      //get dependencies
      for (dep <- op.getDependencies) { //foreach dependency
        if (!available.contains(dep)) { //this dependency does not yet exist on this resource
          //add to available list
          available.add(dep)
          //write a getter
          writeGetter(dep, out)
        }
      }
      //write the function call:

      if (op.isInstanceOf[OP_Control]) writeControl(op, out)
      else writeFunctionCall(op, out)

      //write the setter:
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true //only add setter if output will be consumed by another resource
      }
      if (addSetter) {
        syncList.add(op) //add op to list that needs sync generation
        writeSetter(op, out)
      }
    }
  }

  private def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
    out.append("val ")
    out.append(getSym(op))
    out.append(" : ")
    out.append(op.outputType)
    out.append(" = ")
    out.append(op.task)
    out.append('(')
    var first = true
    for (input <- op.getInputs) {
      if (!first) out.append(',') //no comma before first argument
      first = false
      out.append(getSym(input))
    }
    out.append(')')
    out.append('\n')
  }

  private def writeGetter(dep: DeliteOP, out: StringBuilder) {
    out.append("val ")
    out.append(getSym(dep))
    out.append(" : ")
    out.append(dep.outputType)
    out.append(" = Executable")
    out.append(dep.scheduledResource)
    out.append(".get")
    out.append(dep.id)
    out.append('\n')
  }

  private def writeSetter(op: DeliteOP, out: StringBuilder) {
    out.append(getSync(op))
    out.append(".set(")
    if (op.isInstanceOf[OP_Control] && op.outputType == "Unit") out.append("()") else out.append(getSym(op))
    out.append(')')
    out.append('\n')
  }

  private[codegen] def addSync(list: ArrayList[DeliteOP], out: StringBuilder) {
    val iter = list.iterator
    while (iter.hasNext) { //foreach op
      val op = iter.next
      //add a public get method
      writePublicGet(op, out)
      //add a private sync object
      writeSyncObject(op, out)
    }
  }

  private def writePublicGet(op: DeliteOP, out: StringBuilder) {
    out.append("def get")
    out.append(op.id)
    out.append(" : ")
    out.append(op.outputType)
    out.append(" = ")
    out.append(getSync(op))
    out.append(".get\n")
  }

  private def writeSyncObject(op: DeliteOP, out: StringBuilder) {
    //the header
    out.append("private object ")
    out.append(getSync(op))
    out.append( " {\n")

    //the state
    out.append("private val numConsumers : Int = ")

    val consumerSet = HashSet.empty[Int]
    for (cons <- op.getConsumers) consumerSet += cons.scheduledResource
    consumerSet -= op.scheduledResource
    out.append(consumerSet.size)

    out.append('\n')
    out.append("private var count : Int = 0\n")
    out.append("private val takeIndex = new ThreadLocal[Int]{ override def initialValue = 0 }\n")
    out.append("private var putIndex : Int = 0\n")
    out.append("private var _result : ")
    out.append(op.outputType)
    out.append(" = _\n")

    out.append("private val lock = new ReentrantLock\n")
    out.append("private val notEmpty = lock.newCondition\n")
    out.append("private val notFull = lock.newCondition\n")

    //the getter
    out.append("def get : ")
    out.append(op.outputType)
    out.append(" = { val takeIndex = this.takeIndex.get; val lock = this.lock; lock.lock; try { while (takeIndex == putIndex) { notEmpty.await }; extract(takeIndex) } finally { lock.unlock } }\n")

    out.append("private def extract(takeIndex: Int) : ")
    out.append(op.outputType)
    out.append(" = { val res = _result; this.takeIndex.set(takeIndex + 1); count -= 1; if (count == 0) { _result = null.asInstanceOf[")
    out.append(op.outputType)
    out.append("]; notFull.signal }; res }\n")

    //the setter
    out.append("def set(result : ")
    out.append(op.outputType)
    out.append(") { val lock = this.lock; lock.lock; try { while (count != 0) { notFull.await }; insert(result) } finally { lock.unlock } }\n")

    out.append("private def insert(result: ")
    out.append(op.outputType)
    out.append(") { _result = result; count = numConsumers; putIndex += 1; notEmpty.signalAll }\n")

    //the footer
    out.append('}')
    out.append('\n')
  }

  private def writeControl(op: DeliteOP, out: StringBuilder) {
    op match {
      case beginCond: OP_BeginCondition => {
        out.append("if (")
        out.append(getSym(beginCond.predicate))
        out.append(") {\n")
      }
      case beginElse: OP_BeginElse => {
        out.append("} else {\n")
      }
      case endCond: OP_EndCondition => {
        out.append('}')
        out.append('\n')
      }
      case beginWhile: OP_BeginWhile => {
        val sym = getSym(op).dropRight(1) //the base sym for this while construct
        out.append("var ")
        out.append(sym)
        out.append("p = ")
        out.append(getSym(beginWhile.predicate))
        out.append('\n')

        out.append("while (")
        out.append(sym)
        out.append("p) {\n")
      }
      case endWhile: OP_EndWhile => {
        out.append(getSym(op))
        out.append("p = ")
        out.append(getSym(endWhile.predicate))
        out.append("\n}\n")
      }
      case beginVariant: OP_BeginVariantScope => {
        out.append("val ")
        out.append(getSym(op).dropRight(1)) //the base sym for this variant construct
        out.append(" : ")
        out.append(beginVariant.variantOutputType)
        out.append(" = {\n")
      }
      case endVariant: OP_EndVariantScope => {
        if (op.outputType != "Unit") {
          out.append(getSym(endVariant.result))
          out.append('\n')
        }
        out.append("}\n")
      }
    }
  }

  private def getSym(op: DeliteOP): String = {
    "x"+op.id
  }

  private[codegen] def getSync(op: DeliteOP): String = {
    "Result"+op.id
  }

  private[codegen] def addAccessor(out: StringBuilder) {
    out.append("def self = this\n")
  }

}
