package ppl.delite.framework.datastructures

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer}

trait DeliteCppHostTransfer extends CppHostTransfer {

  val IR: Expressions
  import IR._

  override def emitSend(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            out.append("jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s) {\n".format(quote(sym),remap(sym.tp),quote(sym)))
            out.append("\tj%sArray arr = env->New%sArray(%s->length);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg),quote(sym)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)arr,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)arr, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\treturn arr;\n")
            out.append("}\n")
            out.toString
          case _ => super.emitSend(sym, host)
        }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  override def emitRecv(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            out.append("%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj) {\n".format(remap(sym.tp),quote(sym)))
            out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\t%s *%s = new %s(length);\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tmemcpy(%s->data, dataPtr, length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            out.toString
          case _ => super.emitRecv(sym, host)
        }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  override def emitUpdated(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  override def emitUpdate(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            out.append("void updateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s) {\n".format(quote(sym),remap(sym.tp),quote(sym)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            //out.append("\treturn arr;\n")
            out.append("}\n")
            out.toString
          case _ => super.emitUpdate(sym, host)
        }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

}