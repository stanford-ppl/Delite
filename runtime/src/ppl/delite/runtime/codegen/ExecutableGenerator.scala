package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler.PartialSchedule
import ppl.delite.runtime.Config
import java.util.ArrayDeque
import collection.mutable.{ArrayBuffer, HashSet}

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

abstract class ExecutableGenerator {

  def makeExecutables(schedule: PartialSchedule, kernelPath: String) {
    for (i <- 0 until schedule.numResources) {
      val src = makeExecutable(schedule(i), i, kernelPath)
      ScalaCompile.addSource(src, executableName + i)
    }
  }

  protected def makeExecutable(resource: ArrayDeque[DeliteOP], location: Int, kernelPath: String) = {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added

    //the header
    writeHeader(out, location, kernelPath)

    //the run method
    out.append("def run() {\n")
    addKernelCalls(resource, location, out, new ArrayBuffer[DeliteOP], syncList)
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

  protected def writeHeader(out: StringBuilder, location: Int, kernelPath: String) {
    out.append("import ppl.delite.runtime.codegen.DeliteExecutable\n") //base trait
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    ExecutableGenerator.writePath(kernelPath, out) //package of scala kernels
    out.append("object Executable")
    out.append(location)
    out.append(" extends DeliteExecutable {\n")
  }

  protected def addKernelCalls(resource: ArrayDeque[DeliteOP], location: Int, out: StringBuilder, available: ArrayBuffer[DeliteOP], syncList: ArrayBuffer[DeliteOP]) {
    val iter = resource.iterator
    while (iter.hasNext) { //foreach op
      val op = iter.next
      //add to available list
      available += op

      if (op.isInstanceOf[OP_Nested]) makeNestedFunction(op, location)

      //get dependencies
      for (dep <- op.getDependencies) {
        if (!available.contains(dep)) { //this dependency does not yet exist on this resource
          //add to available list
          available += dep
          //write getter(s) for dep output(s)
          for (sym <- dep.getOutputs)
            writeGetter(dep, sym, location, out)
        }
      }

      //write the function call:
      writeFunctionCall(op, out)

      //write the setter:
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true //only add setter if output will be consumed by another resource
      }
      if (addSetter) {
        syncList += op //add op to list that needs sync generation
        for (sym <- op.getOutputs)
          writeSetter(op, sym, out)
      }
    }
  }

  protected def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
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

  protected def writeGetter(dep: DeliteOP, sym: String, location: Int, out: StringBuilder) {
    out.append("val ")
    out.append(getSym(dep, sym))
    out.append(" : ")
    out.append(dep.outputType(sym))
    out.append(" = ")
    out.append(executableName)
    out.append(dep.scheduledResource)
    out.append(".get")
    out.append(location)
    out.append('_')
    out.append(getSym(dep, sym))
    out.append('\n')
  }

  protected def executableName: String

  protected def writeSetter(op: DeliteOP, sym: String, out: StringBuilder) {
    out.append(getSync(op, sym))
    out.append(".set(")
    out.append(getSym(op, sym))
    out.append(')')
    out.append('\n')
  }

  protected def makeNestedFunction(op: DeliteOP, location: Int) {
    op match {
      case c: OP_Condition => new ConditionGenerator(c, location).makeExecutable()
      case w: OP_While => new WhileGenerator(w, location).makeExecutable()
      case v: OP_Variant => new VariantGenerator(v, location).makeExecutable()
      case err => error("Unrecognized nested OP type: " + err.getClass.getSimpleName)
    }
  }

  protected def addSync(list: ArrayBuffer[DeliteOP], out: StringBuilder) {
    for (op <- list) {
      for (sym <- op.getOutputs) {
        //add a public get method
        writePublicGet(op, sym, out)
        //add a private sync object
        writeSyncObject(op, sym, out)
      }
    }
  }

  protected def writePublicGet(op: DeliteOP, sym: String, out: StringBuilder) {
    val consumerSet = calculateConsumerSet(op)
    for (location <- consumerSet) {
      out.append("def get")
      out.append(location)
      out.append('_')
      out.append(getSym(op, sym))
      out.append(" : ")
      out.append(op.outputType(sym))
      out.append(" = ")
      out.append(getSync(op, sym))
      out.append(".get")
      out.append(location)
      out.append('\n')
    }
  }

  protected def writeSyncObject(op: DeliteOP, sym: String, out: StringBuilder) {
    //the header
    out.append("private object ")
    out.append(getSync(op, sym))
    out.append( " {\n")

    //the state
    val consumerSet = calculateConsumerSet(op)
    val numConsumers = consumerSet.size

    out.append("private var count : Int = 0\n")
    for (cons <- consumerSet) {
      out.append("private var takeIndex")
      out.append(cons)
      out.append(" : Int = 0\n")
    }
    out.append("private var putIndex : Int = 0\n")
    out.append("private var _result : ")
    out.append(op.outputType(sym))
    out.append(" = _\n")

    out.append("private val lock = new ReentrantLock\n")
    out.append("private val notEmpty = lock.newCondition\n")
    out.append("private val notFull = lock.newCondition\n")

    //the getters
    for (cons <- consumerSet) {
      out.append("def get")
      out.append(cons)
      out.append(" : ")
      out.append(op.outputType(sym))
      out.append(" = { val takeIndex = takeIndex")
      out.append(cons)
      out.append("; val lock = this.lock; lock.lock; try { while (takeIndex == putIndex) { notEmpty.await }; extract")
      out.append(cons)
      out.append(" } finally { lock.unlock } }\n")

      out.append("private def extract")
      out.append(cons)
      out.append(" : ")
      out.append(op.outputType(sym))
      out.append(" = { val res = _result; takeIndex")
      out.append(cons)
      out.append("+= 1; count -= 1; if (count == 0) { _result = null.asInstanceOf[")
      out.append(op.outputType(sym))
      out.append("]; notFull.signal }; res }\n")
    }

    //the setter
    out.append("def set(result : ")
    out.append(op.outputType(sym))
    out.append(") { val lock = this.lock; lock.lock; try { while (count != 0) { notFull.await }; insert(result) } finally { lock.unlock } }\n")

    out.append("private def insert(result: ")
    out.append(op.outputType(sym))
    out.append(") { _result = result; count = ")
    out.append(numConsumers)
    out.append("; putIndex += 1; notEmpty.signalAll }\n")

    //the footer
    out.append('}')
    out.append('\n')
  }

  protected def calculateConsumerSet(op: DeliteOP) = {
    val consumerSet = HashSet.empty[Int]
    for (cons <- op.getConsumers) consumerSet += cons.scheduledResource
    consumerSet -= op.scheduledResource
    consumerSet
  }

  protected def getSym(op: DeliteOP, name: String): String = {
    "x"+name
  }

  protected def getSync(op: DeliteOP, name: String): String = {
    "Result"+name
  }

  protected def addAccessor(out: StringBuilder) {
    out.append("def self = this\n")
  }

}

object ExecutableGenerator {
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
