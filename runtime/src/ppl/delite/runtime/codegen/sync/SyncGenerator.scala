package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import collection.mutable.{HashSet, ArrayBuffer}
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ExecutableGenerator}
import ppl.delite.runtime.scheduler.OpList

/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait SyncGenerator {

  def addSync(s: Sync) = s match {
    case s:SendData => sendData(s)
    case s:ReceiveData => receiveData(s)
    case s:SendView => sendView(s)
    case s:ReceiveView => receiveView(s)
    case s:Notify => sendSignal(s)
    case s:Await => awaitSignal(s)
    case s:SendUpdate => sendUpdate(s)
    case s:ReceiveUpdate => receiveUpdate(s)
    case _ => throw new IllegalArgumentException("Unrecognized Sync type: " + s)
  }

  protected def sendData(s: SendData) { } //TODO: should probably throw an exception
  protected def receiveData(s: ReceiveData) { notImplemented(s) }
  protected def sendView(s: SendView) { }
  protected def receiveView(s: ReceiveView) { notImplemented(s) }
  protected def sendSignal(s: Notify) { }
  protected def awaitSignal(s: Await) { notImplemented(s) }
  protected def sendUpdate(s: SendUpdate) { }
  protected def receiveUpdate(s: ReceiveUpdate) { }

  private def notImplemented(s: Sync) = sys.error("don't know how to synchronize " + s)

  protected def getOpSync(op: DeliteOP) = getSync(op, "op_"+op.id)
  protected def getSync(op: DeliteOP, name: String): String = { "Result_"+op.id+"_"+name }

  protected def mangledName(name: String) = name.replaceAll("\\s","").map(c => if(!c.isDigit && !c.isLetter) '_' else c) 
}

trait SyncObjectGenerator extends SyncGenerator with ExecutableGenerator {

  protected val sync: ArrayBuffer[Send]

  def executableNamePrefix = "Sync_"

  protected def addSyncObject(): Unit

  def makeSyncObjects {
    writeHeader()
    addSyncObject()
    writeFooter()
    addSource(out.toString)
  }
}

