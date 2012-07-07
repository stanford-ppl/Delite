package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops.{Notify, SendView, Send}
import ppl.delite.runtime.codegen.CppExecutableGenerator
import ppl.delite.runtime.graph.targets.Targets

trait CppSyncObjectGenerator extends SyncObjectGenerator with CppExecutableGenerator {

  protected def addSyncObject() {
    for (sender <- sync) {
      sender match {
        case s: SendView =>
          writePublicGet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(Targets.Cpp,s.sym))
          SyncObject(s, getSync(s.from, s.sym), s.from.outputType(Targets.Cpp,s.sym))
          writePublicSet(s, getSym(s.from, s.sym), getSync(s.from, s.sym), s.from.outputType(Targets.Cpp,s.sym))
        case s: Notify =>
          writePublicGet(s, getOpSym(s.from), getOpSync(s.from), "void")
          SyncObject(s, getOpSync(s.from), "void")
          writePublicSet(s, getOpSym(s.from), getOpSync(s.from), "void")
      }
    }
  }

  private def consumerSet(sender: Send) = sender.receivers.map(_.to.scheduledResource)

  private def writePublicGet(sender: Send, symName: String, syncName: String, outputType: String) {
    val header = new StringBuilder
    val locations = consumerSet(sender)
    for (location <- locations) {
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
    header.append(" result)")
    CppExecutableGenerator.syncObjects.append(header.toString + ";\n")

    out.append(header.toString)
    out.append("{ ")
    out.append(syncName)
    out.append("::set(result); }")
    out.append('\n')
  }

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
    header.append("static " + outputType + " _result;\n")

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
      header.append("; pthread_mutex_lock(&%1$s::lock); while(takeIndex==%1$s::putIndex) { pthread_cond_wait(&%1$s::notEmpty,&%1$s::lock); }; extract".format(syncName))
      header.append(cons)
      header.append("(); pthread_mutex_unlock(&%1$s::lock); }\n".format(syncName))

      header.append("static ")
      header.append(outputType)
      header.append(" extract")
      header.append(cons)
      header.append("(void) { ")
      header.append(" %1$s res = %2$s::_result; %2$s::takeIndex".format(outputType, syncName))
      header.append(cons)
      header.append("+= 1; %1$s::count -= 1; if (%1$s::count == 0) { /* TODO: free _result */".format(syncName))
      header.append(" pthread_cond_signal(&%1$s::notFull); }; return res; }\n".format(syncName))
    }

    //the setter
    header.append("static void set(")
    header.append(outputType)
    header.append(" result) {")
    header.append(" pthread_mutex_lock(&%1$s::lock); while (%1$s::count != 0) { pthread_cond_wait(&%1$s::notFull,&%1$s::lock); }; insert(result); pthread_mutex_unlock(&%1$s::lock); }\n".format(syncName))

    header.append("static void insert(")
    header.append(outputType)
    header.append(" result) {")
    header.append(" %1$s::_result = result; %1$s::count = ".format(syncName))
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
    out.append(outputType + " " + syncName + "::_result = NULL;\n")
    out.append("pthread_mutex_t " + syncName + "::lock = PTHREAD_MUTEX_INITIALIZER;\n")
    out.append("pthread_cond_t " + syncName + "::notEmpty = PTHREAD_COND_INITIALIZER;\n")
    out.append("pthread_cond_t " + syncName + "::notFull = PTHREAD_COND_INITIALIZER;\n")

  }
}