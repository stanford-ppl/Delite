package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.codegen.ScalaExecutableGenerator
import ppl.delite.runtime.scheduler.OpHelper._
import ppl.delite.runtime.graph.targets._
import scala.collection.mutable.HashSet

trait ScalaSyncObjectGenerator extends SyncObjectGenerator with ScalaExecutableGenerator {

  private val generatedSyncObjects: HashSet[String] = HashSet[String]()

  protected def addSyncObject() {
    for (sender <- sync) {
      sender match {
        case s: SendData =>
          writePublicGet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(s.sym))
          SyncObject(s, getSync(s.from, s.sym), s.from.outputType(s.sym))
          writePublicSet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(s.sym))
        case s: SendView =>
          writePublicGet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(s.sym))
          SyncObject(s, getSync(s.from, s.sym), s.from.outputType(s.sym))
          writePublicSet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(s.sym))
        case s: Notify =>
          if(!generatedSyncObjects.contains(getOpSync(s.from))) {
            writePublicGet(s, getOpSym(s.from), getOpSync(s.from), "Unit")
            SyncObject(s, getOpSync(s.from), "Unit")
            writePublicSet(s, getOpSym(s.from), getOpSync(s.from), "Unit")
            generatedSyncObjects += getOpSync(s.from)
          }
        case s: SendUpdate =>
          if(!generatedSyncObjects.contains(getSync(s.from, s.sym))) {
            writePublicGet(s, getOpSym(s.from) + getSym(s.from, s.sym), getSync(s.from, s.sym), "Unit")
            SyncObject(s, getSync(s.from, s.sym), "Unit")
            writePublicSet(s, getOpSym(s.from) + getSym(s.from, s.sym), getSync(s.from, s.sym), "Unit")
            generatedSyncObjects += getSync(s.from, s.sym)
          }
      }
    }
  }

  //private def consumerSet(sender: Send) = sender.receivers.map(_.to.scheduledResource)
  def consumerSet(sender: Send) =
    if (Targets.getHostTarget(scheduledTarget(sender.from))==Targets.Cpp) sender.receivers.map(_.to).filter(op=>Targets.getHostTarget(scheduledTarget(op))==Targets.Scala).map(_.scheduledResource)
    else sender.receivers.map(_.to.scheduledResource)

  private def writePublicGet(sender: Send, symName: String, syncName: String, outputType: String) {
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

  private def writePublicSet(sender: Send, symName: String, syncName: String, outputType: String) {
    out.append("def set")
    out.append('_')
    out.append(symName)
    out.append("(result : ")
    out.append(outputType)
    out.append(") = ")
    out.append(syncName)
    out.append(".set(result)")
    out.append('\n')
  }

  private def SyncObject(sender: Send, syncName: String, outputType: String) {
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

  override protected def writeHeader() = {
    ScalaExecutableGenerator.writePackage(graph, out)
    ScalaExecutableGenerator.writePath(graph, out)
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    out.append("object ")
    out.append(executableName)
    out.append(" {\n")
  }
}
