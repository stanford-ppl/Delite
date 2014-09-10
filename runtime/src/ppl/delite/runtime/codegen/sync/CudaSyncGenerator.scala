package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OS, Targets}
import ppl.delite.runtime.codegen._
import ppl.delite.runtime.scheduler.OpHelper._
import ppl.delite.runtime.graph.targets.Targets._
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph._

trait CudaToScalaSync extends SyncGenerator with CudaExecutableGenerator with JNIFuncs {

  private val syncList = new ArrayBuffer[Send]
  
  override protected def receiveData(s: ReceiveData) {
    getHostTarget(scheduledTarget(s.sender.from)) match {
      case Targets.Scala => writeGetter(s.sender.from, s.sender.sym, s.to, false)
    }
  }

  override protected def receiveView(s: ReceiveView) {
    getHostTarget(scheduledTarget(s.sender.from)) match {
      case Targets.Scala => writeGetter(s.sender.from, s.sender.sym, s.to, true)
      case _ => super.receiveView(s)
    }
  }

  override protected def awaitSignal(s: Await) {
    getHostTarget(scheduledTarget(s.sender.from)) match {
      case Targets.Scala => writeAwaiter(s.sender.from)
      case _ => super.awaitSignal(s)
    }
  }

  override protected def receiveUpdate(s: ReceiveUpdate) {
    getHostTarget(scheduledTarget(s.sender.from)) match {
      case Targets.Scala => 
        s.sender.from.mutableInputsCondition.get(s.sender.sym) match {
          case Some(lst) => 
            out.append("if(")
            out.append(lst.map(c => c._1.id.split('_').head + "_cond=="+c._2).mkString("&&"))
            out.append(") {\n")
            writeAwaiter(s.sender.from, s.sender.sym); writeRecvUpdater(s.sender.from, s.sender.sym); 
            out.append("}\n")
          case _ => 
            writeAwaiter(s.sender.from, s.sender.sym); writeRecvUpdater(s.sender.from, s.sender.sym);
        } 
      case _ => super.receiveUpdate(s)
    }
  }

  override protected def sendData(s: SendData) {
    if (s.receivers.map(_.to).filter(r => getHostTarget(scheduledTarget(r)) == Targets.Scala).nonEmpty) {
      writeSetter(s.from, s.sym, false)
      syncList += s
    }
    //else {
    super.sendData(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  override protected def sendView(s: SendView) {
    if (s.receivers.map(_.to).filter(r => getHostTarget(scheduledTarget(r)) == Targets.Scala).nonEmpty) {
      writeSetter(s.from, s.sym, true)
      syncList += s
    }
    //else {
      super.sendView(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  override protected def sendSignal(s: Notify) {
    if (s.receivers.map(_.to).filter(r => getHostTarget(scheduledTarget(r)) == Targets.Scala).nonEmpty) {
      writeNotifier(s.from)
      syncList += s
    }
    //else {
      super.sendSignal(s)  // TODO: always call super? (when multiple receivers have different host types)
    //}
  }

  override protected def sendUpdate(s: SendUpdate) {
    if (s.receivers.map(_.to).filter(r => getHostTarget(scheduledTarget(r)) == Targets.Scala).nonEmpty) {
      s.from.mutableInputsCondition.get(s.sym) match {
        case Some(lst) => 
          out.append("if(")
          out.append(lst.map(c => c._1.id.split('_').head + "_cond=="+c._2).mkString("&&"))
          out.append(") {\n")
          writeSendUpdater(s.from, s.sym)
          writeNotifier(s.from, s.sym)
          out.append("}\n")
        case _ => 
          writeSendUpdater(s.from, s.sym)
          writeNotifier(s.from, s.sym)
      }
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
    val devType = dep.outputType(Targets.Cuda, sym)
    val hostType = dep.outputType(Targets.Cpp, sym)
    if (view) {
      out.append("%s %s%s = recvViewCPPfromJVM_%s(env%s,%s);\n".format(hostType,ref,getSymHost(dep,sym),mangledName(hostType),location,getSymCPU(sym)))
      out.append("%s %s%s = sendCuda_%s(%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
    }
    else if(isPrimitiveType(dep.outputType(sym))) {
      out.append("%s %s = (%s)%s;\n".format(devType,getSymHost(dep,sym),devType,getSymCPU(sym)))
      out.append("%s %s = (%s)%s;\n".format(devType,getSymDevice(dep,sym),devType,getSymHost(dep,sym)))
    }
    else {
      out.append("%s %s%s = recvCPPfromJVM_%s(env%s,%s);\n".format(hostType,ref,getSymHost(dep,sym),mangledName(hostType),location,getSymCPU(sym)))
      //FIXIT: Using the length symbol for transpose is not safe in general because the symbol may not be emitted yet
      if(Config.gpuOptTrans) {
        // Use transpose copy
        dep.stencilOrElse(sym)(Empty) match {
          case Interval(start,stride,length) => out.append("%s %s%s = sendCudaTrans_%s(%s,%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym),getSymDevice(dep,length.trim)))
          case _ => out.append("%s %s%s = sendCuda_%s(%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
        }
      }
      else
        out.append("%s %s%s = sendCuda_%s(%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
      out.append("cudaMemoryMap->insert(std::pair<void*,std::list<void*>*>(")
      out.append(getSymDevice(dep,sym))
      out.append(",lastAlloc));\n")
      out.append("lastAlloc = new std::list<void*>();\n")
    }
  }

  private def writeAwaiter(dep: DeliteOP, sym: String = "") {
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
    if(sym == "") out.append(getOpSym(dep))
    else out.append(getOpSym(dep)+getSym(dep,sym))
    out.append("\",\"()V")
    out.append("\"));\n")
  }

  private def writeRecvUpdater(dep: DeliteOP, sym:String) {
    val hostType = dep.inputType(Targets.Cpp, sym)
    val devType = dep.inputType(Targets.Cuda, sym)
    assert(!isPrimitiveType(dep.inputType(sym)))
    out.append("recvUpdateCPPfromJVM_%s(env%s,%s,%s);\n".format(mangledName(hostType),location,getSymCPU(sym),getSymHost(dep,sym)))
    out.append("sendUpdateCuda_%s(%s, %s);\n".format(mangledName(devType),getSymDevice(dep,sym),getSymHost(dep,sym)))
  }

  private def writeSetter(op: DeliteOP, sym: String, view: Boolean) {
    val hostType = op.outputType(Targets.Cpp, sym) 
    val devType = op.outputType(Targets.Cuda, sym)
    if (view) {
      out.append("%s *%s = recvCuda_%s(%s);\n".format(hostType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = sendViewCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(hostType),location,getSymHost(op,sym)))
    }
    else if(isPrimitiveType(op.outputType(sym)) && op.isInstanceOf[OP_Nested]) {
      out.append("%s %s = (%s)%s;\n".format(hostType,getSymHost(op,sym),hostType,getSymDevice(op,sym)))
      out.append("%s %s = (%s)%s;\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),getJNIType(op.outputType(sym)),getSymHost(op,sym)))
    }
    else if(isPrimitiveType(op.outputType(sym))) {
      out.append("%s %s = recvCuda_%s(%s);\n".format(hostType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = (%s)%s;\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),getJNIType(op.outputType(sym)),getSymHost(op,sym)))      
    }
    else if(devType.startsWith("DeliteArray<")) {
      devType match { //TODO: Fix this for nested object types
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" => 
          out.append("Host%s *%s = recvCuda_%s(%s);\n".format(devType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
        case _ => //DeliteArrayObject Type
          out.append("HostDeliteArray< Host%s  *%s = recvCuda_%s(%s);\n".format(devType.drop(13),getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      }
      out.append("%s %s = sendCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(devType),location,getSymHost(op,sym)))
    }
    else {
      out.append("%s *%s = recvCuda_%s(%s);\n".format(hostType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = sendCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(hostType),location,getSymHost(op,sym)))
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

  private def writeNotifier(op: DeliteOP, sym: String = "") {
    out.append("env")
    out.append(location)
    out.append("->CallStaticVoidMethod(cls")
    out.append(location)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(location)
    out.append(",\"set_")
    if(sym == "") out.append(getOpSym(op))
    else out.append(getOpSym(op)+getSym(op,sym))
    out.append("\",\"(")
    out.append(getJNIArgType("Unit"))
    out.append(")V\"),")
    out.append("boxedUnit")
    out.append(");\n")
  }

  private def writeSendUpdater(op: DeliteOP, sym: String) {
    val devType = op.inputType(Targets.Cuda, sym)
    val hostType = op.inputType(Targets.Cpp, sym)
    assert(!isPrimitiveType(op.inputType(sym)))
    out.append("recvUpdateCuda_%s(%s, %s);\n".format(mangledName(devType),getSymDevice(op,sym),getSymHost(op,sym)))
    out.append("sendUpdateCPPtoJVM_%s(env%s,%s,%s);\n".format(mangledName(hostType),location,getSymCPU(sym),getSymHost(op,sym)))
  }


  override protected def writeSyncObject() {
    //if (syncList.nonEmpty) {
      syncObjectGenerator(syncList, Targets.Scala).makeSyncObjects
    //}
    super.writeSyncObject()
  }
}

trait CudaSyncGenerator extends CudaToScalaSync {

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    //case r: Receive if (getHostTarget(scheduledTarget(r.sender.from)) == Targets.Scala) => addSync(r)
    //case s: Send if (s.receivers.map(_.to).filter(r => getHostTarget(scheduledTarget(r)) == Targets.Scala).nonEmpty) => addSync(s)
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
      out.append(".keys = new std::list< std::pair<void*,bool> >();\n")
    }

    def writeCudaFree(sym: String, isPrim: Boolean) {
      out.append("std::pair<void*,bool> ")
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

    def writeHostRelease(fop: DeliteOP, sym: String) {
      //out.append(getSymHost(fop,sym))
      //out.append("->release();\n")
    }

    //TODO: Separate JVM/Host/Device frees
    writeCudaFreeInit()
    for (f <- m.items) {
      writeCudaFree(f._2, isPrimitiveType(f._1.outputType(f._2)))
      
      if ( (f._1.scheduledResource != location) || (f._1.getConsumers.filter(c => c.scheduledResource!=location && c.getInputs.map(_._2).contains(f._2)).nonEmpty) ) {
        if (!isPrimitiveType(f._1.outputType(f._2)) && f._1.outputType(f._2)!="Unit") {
          writeJVMRelease(f._2)
          writeHostRelease(f._1,f._2)
        }
      }
    }

    //sync on kernel stream (if copied back guaranteed to have completed, so don't need sync on d2h stream)
    out.append(freeItem)
    out.append(".event = addHostEvent(kernelStream);\n")
    out.append("freeList->push(")
    out.append(freeItem)
    out.append(");\n")
  }
}
