package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import collection.mutable.{HashSet, ArrayBuffer}
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ExecutableGenerator}

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

  protected def sendView(s: SendView)
  protected def receiveView(s: ReceiveView)
  protected def sendSignal(s: Notify)
  protected def awaitSignal(s: Await)

  private def notYetImplemented(s: Sync) = throw new RuntimeException(s + " sync is not currently implemented")

}

trait ScalaToScalaSync extends SyncGenerator with ScalaExecutableGenerator {

  private val syncList = new ArrayBuffer[Send]

  protected def receiveData(sym: String, from: DeliteOP, to: DeliteOP)

  protected def receiveView(s: ReceiveView) {
    writeGetter(s.sender.from, s.sender.sym)
  }

  protected def awaitSignal(s: Await) {
    writeAwaiter(s.sender.from)
  }

  protected def sendView(s: SendView) {
    writeSetter(s.from, s.sym)
    syncList += s
  }

  protected def sendSignal(s: Notify) {
    writeNotifier(s.from)
    syncList += s
  }

  protected def writeGetter(dep: DeliteOP, sym: String) {
    out.append("val ")
    out.append(getSym(dep, sym))
    out.append(" : ")
    out.append(dep.outputType(sym))
    out.append(" = ")
    out.append(executableName(dep.scheduledResource))
    out.append(".get")
    out.append(location)
    out.append('_')
    out.append(getSym(dep, sym))
    out.append('\n')
  }

  protected def writeAwaiter(dep: DeliteOP) {
    out.append(executableName(dep.scheduledResource))
    out.append(".get")
    out.append(location)
    out.append('_')
    out.append(getOpSym(dep))
    out.append('\n')
  }

  protected def writeSetter(op: DeliteOP, sym: String) {
    out.append(getSync(op, sym))
    out.append(".set(")
    out.append(getSym(op, sym))
    out.append(')')
    out.append('\n')
  }

  protected def writeNotifier(op: DeliteOP) {
    out.append(getOpSync(op))
    out.append(".set(())\n")
  }

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    case s: Sync if() => addSync(s) //TODO: if sync companion also Scala
    case _ => super.makeNestedFunction(op)
  }

  override protected def writeHeader() = {
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    super.writeHeader()
  }

  override protected def writeFooter() {
    addSync()
    super.writeFooter()
  }

  protected def getOpSym(op: DeliteOP) = getSym(op, "op_"+op.id)
  protected def getOpSync(op: DeliteOP) = getSync(op, "op_"+op.id)

  protected def addSync() {
    for (sender <- syncList) {
      sender match {
        case s: SendView =>
          writePublicGet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(s.sym))
          writeSyncObject(s, getSync(s.from, s.sym), s.from.outputType(s.sym))
        case s: Notify =>
          writePublicGet(s, getOpSym(s.from), getOpSync(s.from), "Unit")
          writeSyncObject(s, getOpSync(s.from), "Unit")
      }
    }
  }

  protected def consumerSet(sender: Send) = sender.receivers.map(_.to.scheduledResource)

  protected def writePublicGet(sender: Send, symName: String, syncName: String, outputType: String) {
    val locations = consumerSet(sender)
    for (location <- locations) {
      out.append("def get")
      out.append(location)
      out.append('_')
      out.append(symName)
      out.append(" : ")
      out.append(outputType)
      out.append(" = ")
      out.append(syncName)
      out.append(".get")
      out.append(location)
      out.append('\n')
    }
  }

  protected def writeSyncObject(sender: Send, syncName: String, outputType: String) {
    //the header
    out.append("private object ")
    out.append(syncName)
    out.append( " {\n")

    //the state
    val locations = consumerSet(sender)
    val numConsumers = locations.size

    out.append("private var count : Int = 0\n")
    for (cons <- locations) {
      out.append("private var takeIndex")
      out.append(cons)
      out.append(" : Int = 0\n")
    }
    out.append("private var putIndex : Int = 0\n")
    out.append("private var _result : ")
    out.append(outputType)
    out.append(" = _\n")

    out.append("private val lock = new ReentrantLock\n")
    out.append("private val notEmpty = lock.newCondition\n")
    out.append("private val notFull = lock.newCondition\n")

    //the getters
    for (cons <- locations) {
      out.append("def get")
      out.append(cons)
      out.append(" : ")
      out.append(outputType)
      out.append(" = { val takeIndex = takeIndex")
      out.append(cons)
      out.append("; val lock = this.lock; lock.lock; try { while (takeIndex == putIndex) { notEmpty.await }; extract")
      out.append(cons)
      out.append(" } finally { lock.unlock } }\n")

      out.append("private def extract")
      out.append(cons)
      out.append(" : ")
      out.append(outputType)
      out.append(" = { val res = _result; takeIndex")
      out.append(cons)
      out.append("+= 1; count -= 1; if (count == 0) { _result = null.asInstanceOf[")
      out.append(outputType)
      out.append("]; notFull.signal }; res }\n")
    }

    //the setter
    out.append("def set(result : ")
    out.append(outputType)
    out.append(") { val lock = this.lock; lock.lock; try { while (count != 0) { notFull.await }; insert(result) } finally { lock.unlock } }\n")

    out.append("private def insert(result: ")
    out.append(outputType)
    out.append(") { _result = result; count = ")
    out.append(numConsumers)
    out.append("; putIndex += 1; notEmpty.signalAll }\n")

    //the footer
    out.append('}')
    out.append('\n')
  }

  protected def getSync(op: DeliteOP, name: String): String = {
    "Result"+name
  }

}

trait ScalaSyncGenerator extends ScalaToScalaSync //with ScalaToCppSync
