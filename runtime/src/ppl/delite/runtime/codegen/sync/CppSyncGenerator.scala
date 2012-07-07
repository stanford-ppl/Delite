package ppl.delite.runtime.codegen.sync


import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OS, Targets}
import ppl.delite.runtime.codegen._
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.scheduler.OpHelper._
import ppl.delite.runtime.graph.targets.Targets._

trait CppToScalaSync extends SyncGenerator with CppExecutableGenerator with JNIFuncs {

  private val syncList = new ArrayBuffer[Send]

  override protected def receiveView(s: ReceiveView) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Scala => writeGetter(s.sender.from, s.sender.sym, s.to)
      case _ => super.receiveView(s)
    }
  }

  override protected def awaitSignal(s: Await) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Scala => writeAwaiter(s.sender.from)
      case _ => super.awaitSignal(s)
    }
  }

  override protected def sendView(s: SendView) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) {
      writeSetter(s.from, s.sym)
      syncList += s
    }
    else {
      super.sendView(s)  // TODO: always call super? (when multiple receivers have different host types)
    }
  }

  override protected def sendSignal(s: Notify) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) {
      writeNotifier(s.from)
      syncList += s
    }
    else {
      super.sendSignal(s)  // TODO: always call super? (when multiple receivers have different host types)
    }
  }

  private def writeGetter(dep: DeliteOP, sym: String, to: DeliteOP) {
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
    println("inputtypesmap for " + location + ", " + to + ":" + to.printInputTypesMap)
    out.append("%s %s = recvCPPfromJVM_%s(env%s,%s);\n".format(to.inputType(Targets.Cpp,sym),getSymHost(dep,sym),sym,location,getSymCPU(sym)))
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

  private def writeSetter(op: DeliteOP, sym: String) {
    out.append("%s %s = sendCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),sym,location,getSymHost(op,sym)))
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
    out.append(",env->GetStaticMethodID(cls")
    out.append(location)
    out.append(",\"set_")
    out.append(getOpSym(op))
    out.append("\",\"(")
    out.append(getJNIArgType("Unit"))
    out.append(")V\"),")
    out.append("boxedUnit")
    out.append(");\n")
  }

  override protected def writeSyncObject() {
    //if (syncList.nonEmpty) {
      syncObjectGenerator(syncList, Hosts.Scala).makeSyncObjects
    //}
    super.writeSyncObject()
  }
}

trait CppToCppSync extends SyncGenerator with CppExecutableGenerator with JNIFuncs {

  private val syncList = new ArrayBuffer[Send]

  override protected def receiveView(s: ReceiveView) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Cpp => writeGetter(s.sender.from, s.sender.sym, s.to)
      case _ => super.receiveView(s)
    }
  }

  override protected def awaitSignal(s: Await) {
    getHostType(scheduledTarget(s.sender.from)) match {
      case Hosts.Cpp => writeAwaiter(s.sender.from)
      case _ => super.awaitSignal(s)
    }
  }

  override protected def sendView(s: SendView) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Cpp).nonEmpty) {
      writeSetter(s.from, s.sym)
      syncList += s
    }
    else {
      super.sendView(s)  // TODO: always call super? (when multiple receivers have different host types)
    }
  }

  override protected def sendSignal(s: Notify) {
    if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Cpp).nonEmpty) {
      writeNotifier(s.from)
      syncList += s
    }
    else {
      super.sendSignal(s)  // TODO: always call super? (when multiple receivers have different host types)
    }
  }

  private def writeGetter(dep: DeliteOP, sym: String, to: DeliteOP) {
    out.append(to.inputType(Targets.Cpp,sym))
    out.append(' ')
    out.append(getSymHost(dep, sym))
    out.append(" = ")
    out.append("get")
    out.append(location)
    out.append('_')
    out.append(getSym(dep, sym))
    out.append("();\n")
  }

  private def writeAwaiter(dep: DeliteOP) {
    out.append("get")
    out.append(location)
    out.append('_')
    out.append(getOpSym(dep))
    out.append("();\n")
  }

  private def writeSetter(op: DeliteOP, sym: String) {
    out.append("set_")
    out.append(getSym(op, sym))
    out.append('(')
    out.append(getSymHost(op, sym))
    out.append(')')
    out.append(";\n")
  }

  private def writeNotifier(op: DeliteOP) {
    out.append("set_")
    out.append(getOpSym(op))
    out.append("();\n")
  }

  override protected def writeSyncObject() {
    //if (syncList.nonEmpty) {
      syncObjectGenerator(syncList, Hosts.Cpp).makeSyncObjects
    //}
    super.writeSyncObject()
  }

}

trait CppSyncGenerator extends CppToScalaSync with CppToCppSync {

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    //case r: Receive if (getHostType(scheduledTarget(r.sender.from)) == Hosts.Scala) => addSync(r)
    //case s: Send if (s.receivers.map(_.to).filter(r => getHostType(scheduledTarget(r)) == Hosts.Scala).nonEmpty) => addSync(s)
    case s: Sync => addSync(s) //TODO: if sync companion also Scala
    case _ => super.makeNestedFunction(op)
  }
}

trait JNIFuncs {
    protected def getJNIType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "void"
      case "Int" => "jint"
      case "Long" => "jlong"
      case "Float" => "jfloat"
      case "Double" => "jdouble"
      case "Boolean" => "jboolean"
      case "Short" => "jshort"
      case "Char" => "jchar"
      case "Byte" => "jbyte"
      //case r if r.startsWith("generated.scala.Ref[") => getJNIType(r.slice(20,r.length-1))
      case _ => "jobject"//all other types are objects
    }
  }

  protected def getJNIArgType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "Lscala/runtime/BoxedUnit;"
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case "Short" => "S"
      case "Char" => "C"
      case "Byte" => "B"
      //case r if r.startsWith("generated.scala.Ref[") => getJNIArgType(r.slice(20,r.length-1))
      case array if array.startsWith("Array[") => "[" + getJNIArgType(array.slice(6,array.length-1))
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    }
  }

  protected def getJNIOutputType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "V"
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case "Short" => "S"
      case "Char" => "C"
      case "Byte" => "B"
      //case r if r.startsWith("generated.scala.Ref[") => getJNIOutputType(r.slice(20,r.length-1))
      case array if array.startsWith("Array[") => "[" + getJNIOutputType(array.slice(6,array.length-1))
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    }
  }

  protected def getJNIFuncType(scalaType: String): String = scalaType match {
    case "Unit" => "Void"
    case "Int" => "Int"
    case "Long" => "Long"
    case "Float" => "Float"
    case "Double" => "Double"
    case "Boolean" => "Boolean"
    case "Short" => "Short"
    case "Char" => "Char"
    case "Byte" => "Byte"
    //case r if r.startsWith("generated.scala.Ref[") => getJNIFuncType(r.slice(20,r.length-1))
    case _ => "Object"//all other types are objects
  }

  protected def getCPrimitiveType(scalaType: String): String = scalaType match {
    case "Unit" => "void"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Short" => "short"
    case "Char" => "char"
    case "Byte" => "char"
    //case r if r.startsWith("generated.scala.Ref[") => getCPrimitiveType(r.slice(20,r.length-1))
    case other => error(other + " is not a primitive type")
  }

  protected def isPrimitiveType(scalaType: String): Boolean = scalaType match {
    case "Unit" => true
    case "Int" => true
    case "Long" => true
    case "Float" => true
    case "Double" => true
    case "Boolean" => true
    case "Short" => true
    case "Char" => true
    case "Byte" => true
    //case r if r.startsWith("generated.scala.Ref[") => isPrimitiveType(r.slice(20,r.length-1))
    case _ => false
  }
}