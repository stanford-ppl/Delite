package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OS, Targets}
import ppl.delite.runtime.codegen._
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.scheduler.OpHelper._
import ppl.delite.runtime.graph.targets.Targets._

trait CudaToScalaSync extends SyncGenerator with CudaExecutableGenerator with JNIFuncs {

  private val syncList = new ArrayBuffer[Send]
  private def mangledName(name: String) = name.map(c => if(!c.isDigit && !c.isLetter) '_' else c) 
  
  override protected def receiveData(s: ReceiveData) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Scala => writeGetter(s.sender.from, s.sender.sym, s.to, false)
    }
  }

  override protected def receiveView(s: ReceiveView) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Scala => writeGetter(s.sender.from, s.sender.sym, s.to, true)
      case _ => super.receiveView(s)
    }
  }

  override protected def awaitSignal(s: Await) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Scala => writeAwaiter(s.sender.from)
      case _ => super.awaitSignal(s)
    }
  }

  override protected def receiveUpdate(s: ReceiveUpdate) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Scala => writeRecvUpdater(s.sender.from, s.sender.sym); writeAwaiter(s.sender.from)
      case _ => super.receiveUpdate(s)
    }
  }

  override protected def sendData(s: SendData) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) {
      writeSetter(s.from, s.sym, false)
      syncList += s
    }
    //else {
    super.sendData(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  override protected def sendView(s: SendView) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) {
      writeSetter(s.from, s.sym, true)
      syncList += s
    }
    //else {
      super.sendView(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  override protected def sendSignal(s: Notify) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) {
      writeNotifier(s.from)
      syncList += s
    }
    //else {
      super.sendSignal(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  override protected def sendUpdate(s: SendUpdate) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) {
      writeSendUpdater(s.from, s.sym)
      writeNotifier(s.from)
      syncList += s
    }
    //else {
      super.sendUpdate(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  private def writeGetter(dep: DeliteOP, sym: String, to: DeliteOP, view: Boolean) {
    out.append(getJNIType(dep.outputType(sym)))
    out.append(' ')
    out.append(getSymCPU(sym))
    out.append(" = ")
    out.append("env")
    out.append(location)
    out.append("->CallStatic")
    out.append(getJNIFuncType(dep.outputType(sym)))
    out.append("Method(cls")
    out.append(dep.scheduledResource)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(dep.scheduledResource)
    out.append(",\"get")
    out.append(location)
    out.append('_')
    out.append(getSym(dep,sym))
    out.append("\",\"()")
    out.append(getJNIOutputType(dep.outputType(Targets.Scala,sym)))
    out.append("\"));\n")
    val ref = if (isPrimitiveType(dep.outputType(sym))) "" else "*"
    val devType = CudaExecutableGenerator.typesMap(Targets.Cuda)(sym)
    if (view) {
      out.append("Host%s %s%s = recvViewCPPfromJVM_%s(env%s,%s);\n".format(devType,ref,getSymHost(dep,sym),mangledName(devType),location,getSymCPU(sym)))
      out.append("%s %s%s = sendCuda_%s(%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
    }
    else if(isPrimitiveType(dep.outputType(sym))) {
      out.append("%s %s = (%s)%s;\n".format(devType,getSymHost(dep,sym),devType,getSymCPU(sym)))
      out.append("%s %s = (%s)%s;\n".format(devType,getSymDevice(dep,sym),devType,getSymHost(dep,sym)))
    }
    else {
      out.append("Host%s %s%s = recvCPPfromJVM_%s(env%s,%s);\n".format(devType,ref,getSymHost(dep,sym),mangledName(devType),location,getSymCPU(sym)))
      out.append("%s %s%s = sendCuda_%s(%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
      out.append("cudaMemoryMap->insert(pair<void*,list<void*>*>(")
      out.append(getSymDevice(dep,sym))
      out.append(",lastAlloc));\n")
      out.append("lastAlloc = new list<void*>();\n")
    }
  }

  private def writeAwaiter(dep: DeliteOP) {
    out.append("env")
    out.append(location)
    out.append("->CallStaticVoid")
    out.append("Method(cls")
    out.append(dep.scheduledResource)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(dep.scheduledResource)
    out.append(",\"get")
    out.append(location)
    out.append('_')
    out.append(getOpSym(dep))
    out.append("\",\"()V")
    out.append("\"));\n")
  }

  private def writeRecvUpdater(dep: DeliteOP, sym:String) {
    val devType = CudaExecutableGenerator.typesMap(Targets.Cuda)(sym)
    assert(!isPrimitiveType(dep.inputType(sym)))
    out.append("recvUpdateCPPfromJVM_%s(env%s,%s,%s);\n".format(mangledName(devType),location,getSymCPU(sym),getSymHost(dep,sym)))
    out.append("sendUpdateCuda_%s(%s, %s);\n".format(mangledName(devType),getSymDevice(dep,sym),getSymHost(dep,sym)))
  }

  private def writeSetter(op: DeliteOP, sym: String, view: Boolean) {
    val devType = CudaExecutableGenerator.typesMap(Targets.Cuda)(sym)
    if (view) {
      out.append("Host%s %s = recvCuda_%s(%s);\n".format(devType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s *%s = sendViewCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(devType),location,getSymHost(op,sym)))
    }
    else if(isPrimitiveType(op.outputType(sym))) {
      out.append("%s %s = recvCuda_%s(%s);\n".format(getCPrimitiveType(op.outputType(sym)),getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = (%s)%s;\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),getJNIType(op.outputType(sym)),getSymHost(op,sym)))
    }
    else {
      out.append("Host%s *%s = recvCuda_%s(%s);\n".format(devType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = sendCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(devType),location,getSymHost(op,sym)))
    }

    out.append("env")
    out.append(location)
    out.append("->CallStaticVoidMethod(cls")
    out.append(location)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(location)
    out.append(",\"set_")
    out.append(getSym(op,sym))
    out.append("\",\"(")
    out.append(getJNIArgType(op.outputType(sym)))
    out.append(")V\"),")
    out.append(getSymCPU(sym))
    out.append(");\n")
  }

  private def writeNotifier(op: DeliteOP) {
    out.append("env")
    out.append(location)
    out.append("->CallStaticVoidMethod(cls")
    out.append(location)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(location)
    out.append(",\"set_")
    out.append(getOpSym(op))
    out.append("\",\"(")
    out.append(getJNIArgType("Unit"))
    out.append(")V\"),")
    out.append("boxedUnit")
    out.append(");\n")
  }

  private def writeSendUpdater(op: DeliteOP, sym: String) {
    val devType = CudaExecutableGenerator.typesMap(Targets.Cuda)(sym)
    assert(!isPrimitiveType(op.inputType(sym)))
    out.append("recvUpdateCuda_%s(%s, %s);\n".format(mangledName(devType),getSymDevice(op,sym),getSymHost(op,sym)))
    out.append("sendUpdateCPPtoJVM_%s(env%s,%s,%s);\n".format(mangledName(devType),location,getSymCPU(sym),getSymHost(op,sym)))
  }


  override protected def writeSyncObject() {
    //if (syncList.nonEmpty) {
      syncObjectGenerator(syncList, Hosts.Scala).makeSyncObjects
    //}
    super.writeSyncObject()
  }
}

trait CudaSyncGenerator extends CudaToScalaSync {

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    //case r: Receive if (getHostType(scheduledTarget(r.sender.from)) == Hosts.Scala) => addSync(r)
    //case s: Send if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) => addSync(s)
    case s: Sync => addSync(s) //TODO: if sync companion also Scala
    case m: Free => addFree(m)
    case _ => super.makeNestedFunction(op)
  }

  //TODO: Factor out to memory management features
  private def addFree(m: Free) {
    val op = m.op
    val freeItem = "freeItem_" + op.id

    def writeCudaFreeInit() {
      out.append("FreeItem ")
      out.append(freeItem)
      out.append(";\n")
      out.append(freeItem)
      out.append(".keys = new list< pair<void*,bool> >();\n")
    }

    def writeCudaFree(sym: String, isPrim: Boolean) {
      out.append("pair<void*,bool> ")
      out.append(getSymDevice(op,sym))
      out.append("_pair(")
      out.append(getSymDevice(op,sym))
      out.append(",")
      out.append(isPrim) //Do not free this ptr using free() : primitive type pointers points to device memory
      out.append(");\n")
      out.append(freeItem)
      out.append(".keys->push_back(")
      out.append(getSymDevice(op,sym))
      out.append("_pair);\n")
    }

    def writeJVMRelease(sym: String) {
      out.append("env")
      out.append(location)
      out.append("->DeleteLocalRef(")
      out.append(getSymCPU(sym))
      out.append(");\n")
    }

    //TODO: Separate JVM/Host/Device frees
    writeCudaFreeInit()
    for (f <- m.items) {
      writeCudaFree(f._2, isPrimitiveType(f._1.outputType(f._2)))
      if (f._1.scheduledResource != location) writeJVMRelease(f._2)
    }

    //sync on kernel stream (if copied back guaranteed to have completed, so don't need sync on d2h stream)
    out.append(freeItem)
    out.append(".event = addHostEvent(kernelStream);\n")
    out.append("freeList->push(")
    out.append(freeItem)
    out.append(");\n")
  }
}
