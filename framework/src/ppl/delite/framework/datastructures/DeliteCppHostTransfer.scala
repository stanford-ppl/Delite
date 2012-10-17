package ppl.delite.framework.datastructures

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer, CLikeCodegen}

trait DeliteCppHostTransfer extends CppHostTransfer {
  this: CLikeCodegen =>

  val IR: Expressions
  import IR._

  override def emitSend(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, Ref<%s > *sym)".format(mangledName(remap(tp)),remap(typeArg))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->FindClass(\"generated/scala/Ref$mc%s$sp\");\n".format(JNITypeDescriptor(typeArg)))
        out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg)))
        out.append("\tjobject obj = env->NewObject(cls,mid,sym->get());\n")
        out.append("\treturn obj;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(tp) match {
          case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, Host%s *sym)".format(mangledName(remap(tp)),remap(tp))
            out.append(signature + " {\n")
            out.append("\t%sArray arr = env->New%sArray(sym->length);\n".format(JNIType(typeArg),remapToJNI(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)arr,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, sym->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)arr, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("\treturn arr;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSend(tp, host)
        }
      }
    }
    else
      super.emitSend(tp, host)
  }

  override def emitRecv(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "Ref<%s > *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tRef<%s> *sym = new Ref<%s>(env->Call%sMethod(obj,mid_get));\n".format(remap(typeArg),remap(typeArg),remapToJNI(typeArg)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(tp) match {
          case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "Host%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
            out.append(signature + " {\n")
            out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tHost%s *sym = new Host%s(length);\n".format(remap(tp),remap(tp)))
            out.append("\tmemcpy(sym->data, dataPtr, length*sizeof(%s));\n".format(remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("\treturn sym;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecv(tp, host)
        }
      }
    }
    else
      super.emitRecv(tp,host)
  }

  //TODO: How to implement sendView to JVM?
  override def emitSendView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, Ref<%s > *%s)".format(mangledName(remap(tp)),remap(typeArg),"sym")
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(tp) match {
          case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tassert(false);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendView(tp, host)
        }
      }
    }
    else
      super.emitSendView(tp, host)
  }

  override def emitRecvView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "Ref<%s > *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tRef<%s> *%s = new Ref<%s>(env->Call%sMethod(obj,mid_get));\n".format(remap(typeArg),"sym",remap(typeArg),remapToJNI(typeArg)))
        out.append("\treturn %s;\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(tp) match {
          case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "Host%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
            out.append(signature + " {\n")
            out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tHost%s *%s = new Host%s(dataPtr,length);\n".format(remap(tp),"sym",remap(tp)))
            //out.append("\tmemcpy(%s->data, dataPtr, length*sizeof(%s));\n".format("sym",remap(typeArg)))
            //out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
            out.append("\treturn %s;\n".format("sym"))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecvView(tp, host)
        }
      }
    }
    else
      super.emitRecvView(tp, host)
  }


  override def emitSendUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, Ref<%s > *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_set = env->GetMethodID(cls,\"set$mc%s$sp\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tenv->CallVoidMethod(obj,mid_set,%s->get());\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(tp) match {
          case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(tp, host)
        }
      }
    }
    else
      super.emitSendUpdate(tp, host)
  }

  override def emitRecvUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, Ref<%s > *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\t%s->set(env->CallVoidMethod(obj,mid_get));\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        remap(tp) match {
          case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecvUpdate(tp, host)
        }
      }
    }
    else
      super.emitRecvUpdate(tp, host)
  }

}
