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
    case s:SendData => notYetImplemented(s)
    case s:ReceiveData => notYetImplemented(s)
    case s:SendView => sendView(s)
    case s:ReceiveView => receiveView(s)
    case s:Notify => sendSignal(s)
    case s:Await => awaitSignal(s)
    case s:SendUpdate => notYetImplemented(s)
    case s:ReceiveUpdate => notYetImplemented(s)
    case _ => throw new IllegalArgumentException("Unrecognized Sync type: " + s)
  }

  protected def sendView(s: SendView) { } //{ notYetImplemented(s) }
  protected def receiveView(s: ReceiveView) { } //{ notYetImplemented(s) }
  protected def sendSignal(s: Notify) { } //notYetImplemented(s)
  protected def awaitSignal(s: Await) { } //notYetImplemented(s) }

  private def notYetImplemented(s: Sync) = throw new RuntimeException(s + " sync is not currently implemented")

  protected def getOpSync(op: DeliteOP) = getSync(op, "op_"+op.id)
  protected def getSync(op: DeliteOP, name: String): String = { "Result"+name }
}

trait SyncObjectGenerator extends SyncGenerator with ExecutableGenerator {

  protected val sync: ArrayBuffer[Send]

  def executableNamePrefix = "Sync_"

  protected def addSyncObject(): Unit

  protected[sync] def makeSyncObjects {
    writeHeader()
    addSyncObject()
    writeFooter()
    addSource(out.toString)
  }
}

