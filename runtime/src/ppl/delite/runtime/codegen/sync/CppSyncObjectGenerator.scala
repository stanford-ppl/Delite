package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops.{SendData, Notify, SendView, Send}
import ppl.delite.runtime.codegen.CppExecutableGenerator
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.scheduler.OpHelper._
import ppl.delite.runtime.Config

trait CppSyncObjectGenerator extends SyncObjectGenerator with CppExecutableGenerator {

  protected def addSyncObject() {
    for (sender <- sync) {
      sender match {
        case s: SendData =>
          val outputType = if (isPrimitiveType(s.from.outputType(s.sym))) s.from.outputType(Targets.Cpp,s.sym)
                           else if(s.from.outputType(Targets.Cpp,s.sym).startsWith("std::shared_ptr")) s.from.outputType(Targets.Cpp,s.sym)
                           else s.from.outputType(Targets.Cpp,s.sym) + " *"
          writePublicGet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), outputType)
          SyncObject(s, getSync(s.from, s.sym), outputType)
          writePublicSet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), outputType)

        case s: SendView =>
          val outputType = if (isPrimitiveType(s.from.outputType(s.sym))) s.from.outputType(Targets.Cpp,s.sym)
                           else if(s.from.outputType(Targets.Cpp,s.sym).startsWith("std::shared_ptr")) s.from.outputType(Targets.Cpp,s.sym)
                           else s.from.outputType(Targets.Cpp,s.sym) + " *"
          writePublicGet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), outputType)
          SyncObject(s, getSync(s.from, s.sym), outputType)
          writePublicSet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), outputType)
        case s: Notify =>
          writePublicGet(s, getOpSym(s.from), getOpSync(s.from), "void")
          SyncObject(s, getOpSync(s.from), "void")
          writePublicSet(s, getOpSym(s.from), getOpSync(s.from), "void")
      }
    }
  }

  //private def consumerSet(sender: Send) = sender.receivers.map(_.to.scheduledResource)
  private def consumerSet(sender: Send) = sender.receivers.map(_.to).filter(op=>Targets.getHostTarget(scheduledTarget(op))==Targets.Cpp).map(_.scheduledResource)

  private def writePublicGet(sender: Send, symName: String, syncName: String, outputType: String) {
    val locations = consumerSet(sender)
    for (location <- locations) {
      val header = new StringBuilder
      header.append(outputType)
      header.append(" get")
      header.append(location)
      header.append('_')
      header.append(symName)
      header.append("(void)")
      CppExecutableGenerator.syncObjects.append(header.toString + ";\n")

      out.append(header.toString)
      out.append("{ ")
      out.append("return ")
      out.append(syncName)
      out.append("::get")
      out.append(location)
      out.append("(); ")
      out.append("}\n")
    }
  }

  private def writePublicSet(sender: Send, symName: String, syncName: String, outputType: String) {
    val header = new StringBuilder
    header.append("void set")
    header.append('_')
    header.append(symName)
    header.append('(')
    header.append(outputType)
    if(outputType != "void") header.append(" result")
    header.append(')')
    CppExecutableGenerator.syncObjects.append(header.toString + ";\n")

    out.append(header.toString)
    out.append("{ ")
    out.append(syncName)
    out.append("::set(")
    if(outputType!="void") out.append("result")
    out.append("); }")
    out.append('\n')
  }

  private def rmSharedPtr(tpe: String): String = tpe.replaceAll("std::shared_ptr<","").replaceAll(">","")

  private def SyncObject(sender: Send, syncName: String, outputType: String) {

    val header = new StringBuilder

    //the header
    header.append("class ")
    header.append(syncName)
    header.append( " {\n")
    header.append("public:\n")

    //the state
    val locations = consumerSet(sender)
    val numConsumers = locations.size

    header.append("static int count;\n")
    for (cons <- locations) {
      header.append("static int takeIndex")
      header.append(cons)
      header.append(";\n")
    }
    header.append("static int putIndex ;\n")
    if (outputType != "void") header.append("static " + outputType + " _result;\n")

    header.append("static pthread_mutex_t lock;\n")
    header.append("static pthread_cond_t notEmpty;\n")
    header.append("static pthread_cond_t notFull;\n")

    //the getters
    for (cons <- locations) {
      header.append("static ")
      header.append(outputType)
      header.append(" get")
      header.append(cons)
      header.append("(void) {")
      header.append("int takeIndex = %s::takeIndex".format(syncName))
      header.append(cons)
      header.append("; pthread_mutex_lock(&%1$s::lock); while(takeIndex==%1$s::putIndex) { pthread_cond_wait(&%1$s::notEmpty,&%1$s::lock); }; ".format(syncName))
      if (outputType != "void") {
        header.append(outputType)
        header.append(" result = ")
      }
      header.append("extract")
      header.append(cons)
      header.append("(); pthread_mutex_unlock(&%1$s::lock); ".format(syncName))
      if (outputType != "void") header.append(" return result;")
      header.append("}\n")

      header.append("static ")
      header.append(outputType)
      header.append(" extract")
      header.append(cons)
      header.append("(void) { ")
      if (outputType != "void") header.append("%1$s res = %2$s::_result;".format(outputType,syncName))
      header.append("%s::takeIndex".format(syncName))
      header.append(cons)
      header.append("+= 1; %1$s::count -= 1; if (%1$s::count == 0) { ".format(syncName))
      if (outputType.startsWith("std::shared_ptr")) {
        header.append("%s::_result.reset();".format(syncName))
      }
      header.append(" pthread_cond_signal(&%1$s::notFull); };".format(syncName))
      if (outputType != "void") 
        header.append("return res;")
      header.append(" }\n".format(syncName))
    }

    //the setter
    header.append("static void set(")
    header.append(outputType)
    if (outputType != "void") header.append(" result")
    header.append(") {")
    header.append(" pthread_mutex_lock(&%1$s::lock); while (%1$s::count != 0) { pthread_cond_wait(&%1$s::notFull,&%1$s::lock); };".format(syncName))
    header.append("insert(")
    if (outputType != "void") header.append("result")
    header.append("); ")
    header.append("pthread_mutex_unlock(&%1$s::lock); }\n".format(syncName))

    header.append("static void insert(")
    header.append(outputType)
    if (outputType != "void") header.append(" result")
    header.append(") {")
    if (outputType != "void") { 
      header.append(" %1$s::_result = result;".format(syncName))
    }
    header.append("%1$s::count = ".format(syncName))
    header.append(numConsumers)
    header.append("; %1$s::putIndex += 1; pthread_cond_broadcast(&%1$s::notEmpty); }\n".format(syncName))

    //the footer
    header.append("};")
    header.append('\n')

    // register for header
    CppExecutableGenerator.syncObjects.append(header.toString)

    // initialization of static variables
    out.append("int " + syncName + "::count = 0;\n")
    for (cons <- locations) {
      out.append("int ")
      out.append(syncName)
      out.append("::takeIndex")
      out.append(cons)
      out.append(" = 0;\n")
    }
    out.append("int " + syncName + "::putIndex = 0;\n")
    if (outputType != "void") 
      out.append(outputType + " " + syncName + "::_result;\n")
    out.append("pthread_mutex_t " + syncName + "::lock = PTHREAD_MUTEX_INITIALIZER;\n")
    out.append("pthread_cond_t " + syncName + "::notEmpty = PTHREAD_COND_INITIALIZER;\n")
    out.append("pthread_cond_t " + syncName + "::notFull = PTHREAD_COND_INITIALIZER;\n")

  }
}
