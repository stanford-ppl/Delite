package ppl.delite.framework.datastructures

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer}

trait DeliteCppHostTransfer extends CppHostTransfer {

  val IR: Expressions
  import IR._

  override def emitSend(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (sym.tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, Ref<%s> *%s)".format(quote(sym),remap(typeArg),quote(sym))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->FindClass(\"generated/scala/Ref$mc%s$sp\");\n".format(JNITypeDescriptor(typeArg)))
        out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg)))
        out.append("\tjobject obj = env->NewObject(cls,mid,%s->get());\n".format(quote(sym)))
        out.append("\treturn obj;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tj%sArray arr = env->New%sArray(%s->length);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg),quote(sym)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)arr,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)arr, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\treturn arr;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSend(sym, host)
        }
      }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  override def emitRecv(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (sym.tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "Ref<%s> *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tRef<%s> *%s = new Ref<%s>(env->Call%sMethod(obj,mid_get));\n".format(remap(typeArg),quote(sym),remap(typeArg),remapToJNI(typeArg)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\t%s *%s = new %s(length);\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tmemcpy(%s->data, dataPtr, length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecv(sym, host)
        }
      }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  //TODO: How to implement sendView to JVM?
  override def emitSendView(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (sym.tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, Ref<%s> *%s)".format(quote(sym),remap(typeArg),quote(sym))
        out.append(signature + " {\n")
        out.append("//Cannot Implement this!")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("//Cannot Implement this!")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSend(sym, host)
        }
      }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  override def emitRecvView(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (sym.tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "Ref<%s> *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tRef<%s> *%s = new Ref<%s>(env->Call%sMethod(obj,mid_get));\n".format(remap(typeArg),quote(sym),remap(typeArg),remapToJNI(typeArg)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\t%s *%s = new %s(%s->dataPtr,length);\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            //out.append("\tmemcpy(%s->data, dataPtr, length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
            //out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecv(sym, host)
        }
      }
    }
    else if (host == Hosts.CPP) {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new Exception("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  override def emitRecvUpdate(sym: Sym[Any], host: Hosts.Value): (String,String) = {
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

  override def emitSendUpdate(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (sym.tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, Ref<%s> *%s)".format(quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_set = env->GetMethodID(cls,\"set$mc%s$sp\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tenv->CallVoidMethod(obj,mid_set,%s->get());\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(sym.tp) match {
          case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(sym, host)
        }
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