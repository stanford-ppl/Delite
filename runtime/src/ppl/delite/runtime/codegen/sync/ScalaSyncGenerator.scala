package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import collection.mutable.{HashSet, ArrayBuffer}
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ExecutableGenerator}
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.graph.targets.Targets

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

  override def sendData(s: SendData) {
    if (Targets.isPrimitiveType(s.from.outputType(s.sym))) {
      writeSetter(s.from, s.sym)
      syncList += s
    }
    else
      sys.error(s.from.id + " send data copy for object types not yet implemented")
  }

  override protected def sendView(s: SendView) {
    writeSetter(s.from, s.sym)
    syncList += s
  }

  override protected def sendSignal(s: Notify) {
    writeNotifier(s.from)
    syncList += s
  }

  private def writeGetter(dep: DeliteOP, sym: String) {
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
  }

  private def writeAwaiter(dep: DeliteOP) {
    out.append("Sync_" + executableName(dep.scheduledResource))
    out.append(".get")
    out.append(location)
    out.append('_')
    out.append(getOpSym(dep))
    out.append('\n')
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

  private def writeNotifier(op: DeliteOP) {
    out.append("Sync_" + executableName(op.scheduledResource))
    out.append(".set_")
    out.append(getOpSym(op))
    out.append("(())\n")
  }

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    case s: Sync => addSync(s) //TODO: if sync companion also Scala
    case _ => super.makeNestedFunction(op)
  }

  protected def writeSyncObject() {
    //if (syncList.nonEmpty) {
      syncObjectGenerator(syncList, Hosts.Scala).makeSyncObjects
    //}
  }
}


trait ScalaSyncGenerator extends ScalaToScalaSync //with ScalaToCppSync

