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

/**
 * [COMMENT TODO] What does this trait do?
 * Base trait for code generation of all Sync nodes:
 * [[SendData]], [[ReceiveData]], [[SendView]], [[ReceiveView]],
 * [[Notify]], [[Await]], [[SendUpdate]], [[ReceiveUpdate]]
 */
trait SyncGenerator {

  /**
   * [COMMENT TODO] What does this method do?
   * Entry method for sync code generation. Pattern-matches
   * on specific kind of Sync node and calls the appropriate
   * function
   * @param s: Sync node to be code-generated
   */
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

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for SendData. To be overridden by every code generator
   */
  protected def sendData(s: SendData) { } //TODO: should probably throw an exception

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for ReceiveData. To be overridden by every code generator
   */
  protected def receiveData(s: ReceiveData) { notImplemented(s) }

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for SendView. To be overridden by every code generator
   */
  protected def sendView(s: SendView) { }

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for ReceiveView. To be overridden by every code generator
   */
  protected def receiveView(s: ReceiveView) { notImplemented(s) }

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for Notify. To be overridden by every code generator
   */
  protected def sendSignal(s: Notify) { }

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for Await. To be overridden by every code generator
   */
  protected def awaitSignal(s: Await) { notImplemented(s) }

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for SendUpdate. To be overridden by every code generator
   */
  protected def sendUpdate(s: SendUpdate) { }

  /**
   * [COMMENT TODO] What does this method do?
   * Generate code for ReceiveUpdate. To be overridden by every code generator
   */
  protected def receiveUpdate(s: ReceiveUpdate) { }

  private def notImplemented(s: Sync) = sys.error("don't know how to synchronize " + s)

  protected def getOpSync(op: DeliteOP) = getSync(op, "op_"+op.id)
  protected def getSync(op: DeliteOP, name: String): String = { "Result_"+op.id+"_"+name }

  protected def mangledName(name: String) = name.replaceAll("\\s","").map(c => if(!c.isDigit && !c.isLetter) '_' else c) 
}

/**
 * [COMMENT TODO] What does this trait do? What is a 'SyncObject'?
 * How is it related to the Sync nodes?
 */
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

