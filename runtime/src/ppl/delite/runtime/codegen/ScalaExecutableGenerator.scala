package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler.{OpList, PartialSchedule}
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.{OS, Targets}
import collection.mutable.ArrayBuffer
import sync._

object ScalaResourceInfo {
  def resourceInfoType = "generated.scala.ResourceInfo"
  def resourceInfoSym = "resourceInfo"
}

trait ScalaExecutableGenerator extends ExecutableGenerator {
  import ScalaResourceInfo._

  protected def addSource(source: String) {
    ScalaCompile.addSource(source, executableName)
  }

  protected def writeJNIInitializer(locations: Set[Int]) {
    for (i <- locations) {
      out.append("import ")
      out.append("Sync_" + executableName(i))
      out.append("._\n")
    }
  }

  protected[codegen] def writeHeader() {
    ScalaExecutableGenerator.writePackage(graph, out)
    out.append("import ppl.delite.runtime.executor.DeliteExecutable\n") //base trait
    out.append("import ppl.delite.runtime.Config\n") //base trait
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import ppl.delite.runtime.profiler.MemoryProfiler\n")
    ScalaExecutableGenerator.writePath(graph, out) //package of scala kernels
    // only emit locations that will have send objects from scala target or to scala target
    val locations = opList.siblings.filter(oplist => !oplist.isEmpty && !oplist.toArray.collect{
      case op: Send if op.from.scheduledOn(Targets.Scala) => op
      case op: Send if op.receivers.exists(_.to.scheduledOn(Targets.Scala)) => op
    }.isEmpty).map(_.resourceID).toSet
    if (!this.isInstanceOf[SyncObjectGenerator]) writeJNIInitializer(locations)
    out.append("object ")
    out.append(executableName)
    out.append(" extends DeliteExecutable {\n")
  }

  protected def writeMethodHeader() {
    out.append("def run() {\n")
    if (location == 0) out.append("PerformanceTimer.start(\"all\", false)\n")
    if (Config.profile) out.append("val threadName = Thread.currentThread.getName()\n")
    out.append("val "+resourceInfoSym+" = "+resourceInfoType+"("+Targets.getRelativeLocation(location)+",Config.numThreads,0,Config.numSlaves)\n")
  }

  protected def writeMethodFooter() {
    out.append("}\n")
  }

  protected def writeFooter() {
    out.append("}\n") //end object
  }

  //TODO: can/should this be factored out? need some kind of factory for each target
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => new ScalaConditionGenerator(c, location, graph).makeExecutable()
    case w: OP_While => new ScalaWhileGenerator(w, location, graph).makeExecutable()
    case err => sys.error("Unrecognized OP type: " + err.getClass.getSimpleName)
  }

  protected[codegen] def writeFunctionCall(op: DeliteOP) {
    def dummyOutput = op.isInstanceOf[OP_MultiLoop] && Targets.getRelativeLocation(location) > 0
    def returnsResult = op.outputType(op.getOutputs.head) == op.outputType || dummyOutput
    def resultName = if (returnsResult && !dummyOutput) getSym(op, op.getOutputs.head) else getOpSym(op)

    if (op.task == null) return //dummy op
    /*
    if (Config.profile) {
      //out.append("MemoryProfiler.pushNameOfCurrKernel(threadName,\"" + getOpId(op) + "\")\n")
      out.append("MemoryProfiler.pushNameOfCurrKernel(Thread.currentThread.getName(),\"" + getOpId(op) + "\")\n")
      //out.append("Predef.println(\"TID: \" + resourceInfo.threadId + \"  ThreadName: \" + Thread.currentThread.getName())\n")


      if (!op.isInstanceOf[OP_MultiLoop]) {
        //out.append("PerformanceTimer.start(\""+op.id+"\", threadName, false)\n")
        out.append("PerformanceTimer.start(\""+op.id+"\", Thread.currentThread.getName(), false)\n")
      }
    }
    */

    if ((Config.profile) && (!op.isInstanceOf[OP_MultiLoop])) {
      out.append("MemoryProfiler.pushNameOfCurrKernel(\"ExecutionThread0\",\"" + op.id + "\")\n")
      out.append("PerformanceTimer.start(\""+op.id+"\", \"ExecutionThread0\", false)\n")
    }

    out.append("val ")
    out.append(resultName)
    out.append(" : ")
    out.append(op.outputType)
    out.append(" = ")
    out.append(op.task)
    out.append('(')
    out.append(resourceInfoSym)
    for ((input, name) <- op.getInputs) {
      out.append(", ")
      out.append(getSym(input, name))
    }
    out.append(")\n")

    /*
    if (Config.profile) {
      if (!op.isInstanceOf[OP_MultiLoop]) {
        //out.append("PerformanceTimer.stop(\""+op.id+"\", threadName, false)\n")
        out.append("PerformanceTimer.stop(\""+op.id+"\", Thread.currentThread.getName(), false)\n")
      }

      //out.append("MemoryProfiler.popNameOfCurrKernel(threadName)\n")
      out.append("MemoryProfiler.popNameOfCurrKernel(Thread.currentThread.getName())\n")
      //out.append("Predef.println(\"TID: \" + resourceInfo.threadId + \"  ThreadName: \" + Thread.currentThread.getName())\n")
    }
    */

    if ((Config.profile) && (!op.isInstanceOf[OP_MultiLoop])) {
      out.append("PerformanceTimer.stop(\""+op.id+"\", \"ExecutionThread0\", false)\n")
      out.append("MemoryProfiler.popNameOfCurrKernel(\"ExecutionThread0\")\n")
    }

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

/*
//<<<<<<< HEAD
  protected def addAccessor() {
    out.append("def self = this\n")
  }

  def getOpId(op: DeliteOP) : String = {
    val isMultiLoop = op.isInstanceOf[OP_MultiLoop]
    var opId = op.id
    if (isMultiLoop && (!op.id.contains('_'))) {
      opId = op.id + "_0"
    }

    return opId
  }
*/
//=======
//>>>>>>> develop
}

class ScalaMainExecutableGenerator(val location: Int, val graph: DeliteTaskGraph)
  extends ScalaExecutableGenerator with ScalaSyncGenerator {

  def executableName(location: Int) = "Executable" + location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value) = {
    target match {
      case Targets.Scala => new ScalaMainExecutableGenerator(location, graph) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Targets.Cpp => new CppMainExecutableGenerator(location, graph) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + target.toString)
    }
  }

  override protected def writeMethodFooter() {
    if (location == 0) out.append("PerformanceTimer.stop(\"all\", false)\n")
    out.append("ppl.delite.runtime.graph.ops.EOP_Global.awaitBarrier();\n")
    out.append("}\n")
  }
}

object ScalaExecutableGenerator {

  def makeExecutables(schedule: PartialSchedule, graph: DeliteTaskGraph) {
    for (sch <- schedule if (sch.size > 0)) {
      val location = sch.peek.scheduledResource
      new ScalaMainExecutableGenerator(location, graph).makeExecutable(sch)
    }
  }

  def getPackage(graph: DeliteTaskGraph) = if (graph.appName == "") "" else graph.appName.toLowerCase+"p"

  private [codegen] def writePackage(graph: DeliteTaskGraph, out: StringBuilder) {
    if (graph.appName != "") out.append("package " + getPackage(graph) + "\n")
  }

  private[codegen] def writePath(graph: DeliteTaskGraph, out: StringBuilder) {
    if (graph.kernelPath == "") return
    val appPath = if (graph.appName == "") "" else graph.appName.toLowerCase+"p."
    out.append("import generated.scala."+appPath+"_\n")
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
