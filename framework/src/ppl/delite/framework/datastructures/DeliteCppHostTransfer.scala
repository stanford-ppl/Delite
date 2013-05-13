package ppl.delite.framework.datastructures

import virtualization.lms.internal.{Targets, Expressions, CppHostTransfer, CLikeCodegen, GenerationFailedException}
import virtualization.lms.common.BaseGenStruct
import virtualization.lms.internal.Targets._

trait DeliteCppHostTransfer extends CppHostTransfer {
  this: CLikeCodegen with CLikeGenDeliteStruct =>

  val IR: Expressions
  import IR._
  
  override def emitSend(tp: Manifest[Any], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %sRef< %s > *sym)".format(mangledName("Ref<"+remapHost(tp)+">"),hostTarget,remapHost(typeArg))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->FindClass(\"generated/scala/Ref$mc%s$sp\");\n".format(JNITypeDescriptor(typeArg)))
        out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg)))
        out.append("\tjobject obj = env->NewObject(cls,mid,sym->get());\n")
        out.append("\treturn obj;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        var args = ""
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            args = args + JNITypeDescriptor(elemtp)
            out.append("\t%s %s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
          else if (encounteredStructs.contains(structName(elemtp))) {
            args = args + "Lgenerated/scala/"+ structName(elemtp) + ";"
            out.append("\t%s %s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
          else { 
            args = args + "["+JNITypeDescriptor(elemtp.typeArguments.head)
            out.append("\t%s %s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
        }
        out.append("\tjclass cls = env->FindClass(\"generated/scala/%s\");\n".format(remapHost(tp).replaceAll(hostTarget,"")))
        out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(%s)V\");\n".format(args))
        out.append("\tjobject obj = env->NewObject(cls,mid,%s);\n".format(encounteredStructs(structName(tp)).map(_._1).mkString(",")))
        out.append("\treturn obj;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")  
        if(isPrimitiveType(typeArg)) {
          out.append("\t%sArray arr = env->New%sArray(sym->length);\n".format(JNIType(typeArg),remapToJNI(typeArg)))
          out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)arr,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\tmemcpy(dataPtr, sym->data, sym->length*sizeof(%s));\n".format(remapHost(typeArg)))
          out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)arr, dataPtr, 0);\n".format(JNIType(typeArg)))
          out.append("\treturn arr;\n")
        }
        else {
          out.append("\tjclass cls = env->FindClass(\"" + JNITypeDescriptor(typeArg) + "\");\n")
          out.append("\tjobjectArray arr = env->NewObjectArray(sym->length,cls,0);\n")
          out.append("\tfor(int i=0; i<sym->length; i++) {\n")
          //TODO: Move the null check to other place? (e.g. struct type transfer and delitearray primitive type transfer)
          out.append("\t\tjobject obj = (sym->data[i]==NULL) ? NULL : sendCPPtoJVM_%s(env, sym->data[i]);\n".format(mangledName(remapHost(typeArg))))
          out.append("\t\tenv->SetObjectArrayElement(arr,i,obj);\n")
          out.append("\t}\n")
          out.append("\treturn arr;\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else 
        super.emitSend(tp, peer)
    }
    else
      super.emitSend(tp, peer)
  }

  override def emitRecv(tp: Manifest[Any], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val signature = "%sRef<%s > *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(hostTarget,remapHost(tp),mangledName("Ref<"+remapHost(tp)+">"))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\t%sRef<%s> *sym = new %sRef<%s>(env->Call%sMethod(obj,mid_get));\n".format(hostTarget,remapHost(typeArg),hostTarget,remapHost(typeArg),remapToJNI(typeArg)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("\t%s *sym = new %s();\n".format(remapHost(tp),remapHost(tp)))
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp)))
            out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format(JNIType(elemtp),elem._1,remapToJNI(elemtp),elem._1))
            out.append("\t%s %s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),elem._1,remapHost(elemtp),elem._1))
          }
          else if (encounteredStructs.contains(structName(elemtp))) { 
            out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()Lgenerated/scala/%s;\");\n".format(elem._1,elem._1,structName(elemtp)))
            out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format("jobject",elem._1,"Object",elem._1))
            out.append("\t%s *%s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
          else {
            out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()[%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp.typeArguments.head)))
            out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format("jobject",elem._1,"Object",elem._1))
            out.append("\t%s *%s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
          out.append("\tsym->%s = %s;\n".format(elem._1,elem._1))
        }
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),mangledName(remapHost(tp)))  
        out.append(signature + " {\n")
        if(isPrimitiveType(typeArg)) {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\t%s *sym = new %s(length);\n".format(remapHost(tp),remapHost(tp)))
          out.append("\tmemcpy(sym->data, dataPtr, length*sizeof(%s));\n".format(remapHost(typeArg)))
          out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
          out.append("\treturn sym;\n")
        }
        else {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          out.append("\t%s *sym = new %s(length);\n".format(remapHost(tp),remapHost(tp)))
          out.append("\tfor(int i=0; i<length; i++) {\n")
          //TODO: Move the null check to other place? (e.g. struct type transfer and delitearray primitive type transfer)
          out.append("\t\tjobject o = env->GetObjectArrayElement((%sArray)obj,i);\n".format(JNIType(typeArg)))
          out.append("\t\tsym->data[i] = (o == NULL)? NULL : recvCPPfromJVM_%s(env, o);\n".format(mangledName(remap(typeArg))))
          out.append("\t}\n")
          out.append("\treturn sym;\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitRecv(tp,peer)
    }
    else
      super.emitRecv(tp,peer)
  }

  //TODO: How to implement sendView to JVM?
  override def emitSendView(tp: Manifest[Any], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %sRef<%s > *sym)".format(mangledName("Ref<"+remapHost(tp)+">"),hostTarget,remapHost(typeArg))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        if (isPrimitiveType(typeArg)) {
          out.append("\tassert(false);\n")
        }
        else {
          out.append("\tassert(false);\n")  
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitSendView(tp, peer)  
    }
    else
      super.emitSendView(tp, peer)
  }

  override def emitRecvView(tp: Manifest[Any], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val signature = "%sRef<%s > *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(hostTarget,remapHost(tp),mangledName("Ref<"+remapHost(tp)+">"))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\t%sRef<%s> *sym = new %sRef<%s>(env->Call%sMethod(obj,mid_get));\n".format(hostTarget,remapHost(typeArg),hostTarget,remapHost(typeArg),remapToJNI(typeArg)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        if (isPrimitiveType(typeArg)) {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\t%s *sym = new %s((%s *)dataPtr,length);\n".format(remapHost(tp),remapHost(tp),remapHost(typeArg)))
          out.append("\treturn sym;\n")  
        } 
        else {
          out.append("\tassert(false);\n")
        }    
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitRecvView(tp, peer)  
    }
    else
      super.emitRecvView(tp, peer)
  }


  override def emitSendUpdate(tp: Manifest[Any], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %sRef<%s > *sym)".format(mangledName("Ref<"+remapHost(tp)+">"),hostTarget,remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_set = env->GetMethodID(cls,\"set$mc%s$sp\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tenv->CallVoidMethod(obj,mid_set,sym->get());\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject &obj, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        var args = ""
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s_$eq\",\"(%s)V\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp)))
            out.append("\tenv->CallVoidMethod(obj,mid_%s,sym->%s);\n".format(elem._1,elem._1))
          }
          else if (encounteredStructs.contains(structName(elemtp))) { 
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()Lgenerated/scala/%s;\");\n".format(elem._1,elem._1,structName(elemtp)))
            out.append("\tjmethodID mid_%s_setter = env->GetMethodID(cls,\"%s_$eq\",\"(Lgenerated/scala/%s;)V\");\n".format(elem._1,elem._1,structName(elemtp)))
            out.append("\tjobject obj_%s = env->CallObjectMethod(obj,mid_%s);\n".format(elem._1, elem._1))
            out.append("\tsendUpdateCPPtoJVM_%s(env,obj_%s,sym->%s);\n".format(mangledName(remapHost(elemtp)),elem._1,elem._1))
            out.append("\tenv->CallVoidMethod(obj,mid_%s_setter,obj_%s);\n".format(elem._1,elem._1))
          }
          else {
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()[%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp.typeArguments.head)))
            out.append("\tjmethodID mid_%s_setter = env->GetMethodID(cls,\"%s_$eq\",\"([%s)V\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp.typeArguments.head)))
            out.append("\tjobject obj_%s = env->CallObjectMethod(obj,mid_%s);\n".format(elem._1, elem._1))
            out.append("\tsendUpdateCPPtoJVM_%s(env,obj_%s,sym->%s);\n".format(mangledName(remapHost(elemtp)),elem._1,elem._1))
            out.append("\tenv->CallVoidMethod(obj,mid_%s_setter,obj_%s);\n".format(elem._1,elem._1))
          }
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject &obj, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        if (isPrimitiveType(typeArg)) {
          out.append("\t%sArray arr = env->New%sArray(sym->length);\n".format(JNIType(typeArg),remapToJNI(typeArg)))
          out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)arr,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\tmemcpy(dataPtr, sym->data, sym->length*sizeof(%s));\n".format(remapHost(typeArg)))
          out.append("\tobj = (jobject)arr;\n")
          out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)arr, dataPtr, 0);\n".format(JNIType(typeArg)))
          //TODO: release obj here?
        }
        else {
          out.append("\tassert(false);\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitSendUpdate(tp, peer)  
    }
    else
      super.emitSendUpdate(tp, peer)
  }

  override def emitRecvUpdate(tp: Manifest[Any], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %sRef<%s > *sym)".format(mangledName("Ref<"+remapHost(tp)+">"),hostTarget,remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tsym->set(env->Call%sMethod(obj,mid_get));\n".format(remapToJNI(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        var args = ""
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp)))
            out.append("\tsym->%s = env->Call%sMethod(obj,mid_%s);\n".format(elem._1,remapToJNI(elemtp),elem._1))
          }
          else if (encounteredStructs.contains(structName(elemtp))) { 
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()Lgenerated/scala/%s;\");\n".format(elem._1,elem._1,structName(elemtp)))
            out.append("\tjobject obj_%s = env->CallObjectMethod(obj,mid_%s);\n".format(elem._1, elem._1))
            out.append("\trecvUpdateCPPfromJVM_%s(env,obj_%s,sym->%s);\n".format(mangledName(remapHost(elemtp)),elem._1,elem._1))
          }
          else { 
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()[%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp.typeArguments.head)))
            out.append("\tjobject obj_%s = env->CallObjectMethod(obj,mid_%s);\n".format(elem._1, elem._1))
            out.append("\trecvUpdateCPPfromJVM_%s(env,obj_%s,sym->%s);\n".format(mangledName(remapHost(elemtp)),elem._1,elem._1))
          }
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        if (isPrimitiveType(typeArg)) {
          out.append("\tsym->length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          out.append("\tsym->data = (%s*)realloc((void*)(sym->data),sizeof(%s)*sym->length);\n".format(remapHost(typeArg),remapHost(typeArg)))
          out.append("\t%s *dataPtr = (%s*)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\tmemcpy(sym->data, dataPtr, sym->length*sizeof(%s));\n".format(remapHost(typeArg)))
          out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))  
        }
        else {
          out.append("\tassert(false);\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitRecvUpdate(tp, peer)
    }
    else
      super.emitRecvUpdate(tp, peer)
  }

  override def JNITypeDescriptor[A](m: Manifest[A]): String = {
    if(isArrayType(m)) {
      "[" + JNITypeDescriptor(m.typeArguments.head)
    }
    else 
      super.JNITypeDescriptor(m)
  }

}
