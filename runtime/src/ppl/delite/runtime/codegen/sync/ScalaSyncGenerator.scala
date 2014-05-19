package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import collection.mutable.{HashSet, ArrayBuffer}
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ExecutableGenerator}
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.Config

trait ScalaToScalaSync extends SyncGenerator with ScalaExecutableGenerator {

  private val syncList = new ArrayBuffer[Send]

  override def receiveData(s: ReceiveData) {
    writeGetter(s.sender.from, s.sender.sym)
  }

  override protected def receiveView(s: ReceiveView) {
    writeGetter(s.sender.from, s.sender.sym)
  }

  override protected def awaitSignal(s: Await) {
    writeAwaiter(s.sender.from)
  }

  override protected def receiveUpdate(s: ReceiveUpdate) {
    s.sender.from.mutableInputsCondition.get(s.sender.sym) match {
      case Some(lst) => 
        out.append("if(")
        //TODO: remove naming convention dependency
        out.append(lst.map(c => "Condition_" + c._1.id.split('_').head + "_" + location + "." + c._1.id.split('_').head + "_cond==" + c._2).mkString("&&"))
        out.append(") {\n")
        writeAwaiter(s.sender.from, s.sender.sym)
        out.append("}\n")
      case _ => 
        writeAwaiter(s.sender.from, s.sender.sym)
    } 
  }

  override def sendData(s: SendData) {
    //if (Targets.isPrimitiveType(s.from.outputType(s.sym))) {
      writeSetter(s.from, s.sym)
      syncList += s
    //}
    //else
    //  sys.error(s.from.id + " send data copy for object types not yet implemented")
  }

  override protected def sendView(s: SendView) {
    writeSetter(s.from, s.sym)
    syncList += s
  }

  override protected def sendSignal(s: Notify) {
    writeNotifier(s.from)
    syncList += s
  }

  override protected def sendUpdate(s: SendUpdate) {
    s.from.mutableInputsCondition.get(s.sym) match {
      case Some(lst) => 
        out.append("if(")
        out.append(lst.map(c => "Condition_" + c._1.id.split('_').head + "_" + location + "." + c._1.id.split('_').head  + "_cond==" + c._2).mkString("&&"))
        out.append(") {\n")
        writeNotifier(s.from, s.sym)
        out.append("}\n")
      case _ => 
        writeNotifier(s.from, s.sym)
    }
    syncList += s
  }

  private def createSyncKernelName(depSym: String, depThread: Int) = {
    var tmp = "-" + depSym + "-" + depThread
    var syncKernelName = "\"__sync-\" + threadName + \"-\" + MemoryProfiler.getNameOfCurrKernel(threadName) + \"" + tmp + "\""
    
    syncKernelName
  }

  private def instrumentSyncStart(syncKernelName: String) {
    var dbgStmt = "PerformanceTimer.start(" + syncKernelName + ", threadName, false)\n"
    out.append(dbgStmt)
  }

  private def instrumentSyncStop(syncKernelName: String) {
    var dbgStmt = "PerformanceTimer.stop(" + syncKernelName + ", threadName, false)\n"
    out.append(dbgStmt)
  }

  private def writeGetter(dep: DeliteOP, sym: String) {
    var syncKernelName = ""
    if (Config.profile) {
      syncKernelName = createSyncKernelName(sym, dep.scheduledResource)
      instrumentSyncStart(syncKernelName)
    }

    out.append("val ")
    out.append(getSym(dep, sym))
    out.append(" : ")
    out.append(dep.outputType(sym))
    out.append(" = ")
    out.append("Sync_" + executableName(dep.scheduledResource))
    out.append(".get")
    out.append(location)
    out.append('_')
    out.append(getSym(dep, sym))
    out.append('\n')

    if (Config.profile) {
      instrumentSyncStop(syncKernelName)
    }
  }

  private def writeAwaiter(dep: DeliteOP, sym: String = "") {
    var syncKernelName = ""
    if (Config.profile) {
      syncKernelName = createSyncKernelName(dep.id, dep.scheduledResource)
      instrumentSyncStart(syncKernelName)
    }
    out.append("Sync_" + executableName(dep.scheduledResource))
    out.append(".get")
    out.append(location)
    out.append('_')
    if(sym == "") out.append(getOpSym(dep))
    else out.append(getOpSym(dep)+getSym(dep,sym))
    out.append('\n')

    if (Config.profile) {
      instrumentSyncStop(syncKernelName)
    }
  }

  private def writeSetter(op: DeliteOP, sym: String) {
    out.append("Sync_" + executableName(op.scheduledResource))
    out.append(".set_")
    out.append(getSym(op, sym))
    out.append('(')
    out.append(getSym(op, sym))
    out.append(')')
    out.append('\n')
  }

  private def writeNotifier(op: DeliteOP, sym: String = "") {
    out.append("Sync_" + executableName(op.scheduledResource))
    out.append(".set_")
    if(sym == "") out.append(getOpSym(op))
    else out.append(getOpSym(op)+getSym(op,sym))
    out.append("(())\n")
  }

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    case s: Sync => addSync(s) //TODO: if sync companion also Scala
    case m: Free => // JVM GC
    case _ => super.makeNestedFunction(op)
  }

  protected def writeSyncObject() {
    //if (syncList.nonEmpty) {
      syncObjectGenerator(syncList, Targets.Scala).makeSyncObjects
    //}
  }

}


trait ScalaSyncGenerator extends ScalaToScalaSync //with ScalaToCppSync

