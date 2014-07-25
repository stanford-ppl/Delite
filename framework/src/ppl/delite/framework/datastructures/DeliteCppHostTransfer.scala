package ppl.delite.framework.datastructures

import virtualization.lms.internal.{Targets, Expressions, CppHostTransfer, CLikeCodegen, GenerationFailedException}
import virtualization.lms.common.BaseGenStruct
import virtualization.lms.internal.Targets._
import ppl.delite.framework.Config

trait DeliteCppHostTransfer extends CppHostTransfer {
  this: CLikeCodegen with CLikeGenDeliteStruct =>

  val IR: Expressions
  import IR._
  
  def nullValue[A](tp: Manifest[A]): String = {
    assert(!isPurePrimitiveType(tp))
    if (remap(tp) == "string") "string(\"\")"
    else if (cppMemMgr == "refcnt") remap(tp) + "(nullptr)"
    else "NULL"
  }

  // Hack: Some cases in the transfer functions, we want to check if the type is a primitive type and not string.
  //       This is a consequence of making string a primitive type to avoid memory management.
  def isPurePrimitiveType[A](tp: Manifest[A]): Boolean = {
    isPrimitiveType(tp) && remap(tp) != "string"
  }

  override def emitSend(tp: Manifest[_], peer: Targets.Value): (String,String) = peer match {
    case Targets.JVM =>
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPurePrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val typename = if (cppMemMgr == "refcnt") wrapSharedPtr(hostTarget+"Ref"+unwrapSharedPtr(remapHost(typeArg)))
                       else hostTarget+"Ref"+remapHost(typeArg)
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(typename),typename,addRef())
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
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("\tif(sym == %s) return NULL;\n".format(nullValue(tp)))
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
          else if (isArrayType(elemtp)) { // TODO: Fix this for cluster
            if(Config.generateSerializable) { //FIX: Is this the cluster mode option?
              if(isPrimitiveType(elemtp.typeArguments.head))
                args = args + "Lppl/delite/runtime/data/DeliteArray" + elemtp.typeArguments.head + ";"
              else 
                args = args + "Lppl/delite/runtime/data/DeliteArrayObject;"
              out.append("\t%s %s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
            }
            else {
              args = args + "["+JNITypeDescriptor(elemtp.typeArguments.head)
              out.append("\t%s %s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
            }
          }
          else if (elemtp.erasure.getSimpleName == "DeliteIndex") {
            args = args + "Lgenerated/scala/container/HashMapImpl;"
            out.append("\t%s %s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
          else throw new GenerationFailedException("DeliteCppHostTransfer: Unknown elem type for struct: " + remap(elemtp))
        }
        if (cppMemMgr == "refcnt")
          out.append("\tjclass cls = env->FindClass(\"generated/scala/%s\");\n".format(unwrapSharedPtr(remapHost(tp)).replaceAll(hostTarget,"")))
        else
          out.append("\tjclass cls = env->FindClass(\"generated/scala/%s\");\n".format(remapHost(tp).replaceAll(hostTarget,"")))
        out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(%s)V\");\n".format(args))
        out.append("\tjobject obj = env->NewObject(cls,mid,%s);\n".format(encounteredStructs(structName(tp)).map(_._1).mkString(",")))
        for(elem <- encounteredStructs(structName(tp)) if !isPrimitiveType(baseType(elem._2))) 
          out.append("\tenv->DeleteLocalRef(" + elem._1 + ");\n")
        out.append("\treturn obj;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")  
        if(isPurePrimitiveType(typeArg)) {
          if(Config.generateSerializable) {
            out.append("\t%sArray arr = env->New%sArray(sym->length);\n".format(JNIType(typeArg),remapToJNI(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)arr,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, sym->data, sym->length*sizeof(%s));\n".format(remapHost(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)arr, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("\tjclass cls = env->FindClass(\"ppl/delite/runtime/data/LocalDeliteArray%s\");\n".format(remapToJNI(typeArg)))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"([%s)V\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tjobject obj = env->NewObject(cls,mid,arr);\n")
            out.append("\tenv->DeleteLocalRef(arr);\n")
            out.append("\treturn obj;\n")
          }
          else {
            out.append("\t%sArray arr = env->New%sArray(sym->length);\n".format(JNIType(typeArg),remapToJNI(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)arr,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, sym->data, sym->length*sizeof(%s));\n".format(remapHost(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)arr, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("\treturn arr;\n")
          }
        }
        else {
          if(Config.generateSerializable) {
            if (encounteredStructs.contains(structName(typeArg))) {
              if (cppMemMgr == "refcnt")
                out.append("\tjclass cls = env->FindClass(\"generated/scala/" + unwrapSharedPtr(remap(typeArg)).replaceAll(deviceTarget,"") + "$\");\n")
              else
                out.append("\tjclass cls = env->FindClass(\"generated/scala/" + remap(typeArg).replaceAll(deviceTarget,"") + "$\");\n")
            }
            else
              out.append("\tjclass cls = env->FindClass(\"" + JNITypeDescriptor(typeArg) + "\");\n")
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"createLocal\",\"(I)Lppl/delite/runtime/data/LocalDeliteArrayObject;\");\n")
            out.append("\tjobject arr = env->CallObjectMethod(cls,mid,sym->length);\n")
            out.append("\tjclass cls1 = env->GetObjectClass(arr);\n")
            out.append("\tjmethodID mid_update = env->GetMethodID(cls1,\"dc_update\",\"(ILjava/lang/Object;)V\");\n")
            out.append("\tfor(int i=0; i<sym->length; i++) {\n")
            if (cppMemMgr == "refcnt" && !isPrimitiveType(typeArg))
              out.append("\t\tjobject obj = (sym->data[i].use_count()==0) ? NULL : sendCPPtoJVM_%s(env, sym->data[i]);\n".format(mangledName(remapHost(typeArg))))
            else
              out.append("\t\tjobject obj = (sym->data[i]==%s) ? NULL : sendCPPtoJVM_%s(env, sym->data[i]);\n".format(nullValue(typeArg),mangledName(remapHost(typeArg))))
            out.append("\t\tenv->CallVoidMethod(arr,mid_update,i,obj);\n")
            out.append("\t\tenv->DeleteLocalRef(obj);\n")
            out.append("\t}\n")
            out.append("\treturn arr;\n")
          }
          else {
            if (encounteredStructs.contains(structName(typeArg))) {
              if (cppMemMgr == "refcnt")
                out.append("\tjclass cls = env->FindClass(\"generated/scala/" + unwrapSharedPtr(remap(typeArg)).replaceAll(deviceTarget,"") + "$\");\n")
              else
                out.append("\tjclass cls = env->FindClass(\"generated/scala/" + remap(typeArg).replaceAll(deviceTarget,"") + "$\");\n")
            }
            else
              out.append("\tjclass cls = env->FindClass(\"" + JNITypeDescriptor(typeArg) + "\");\n")
            out.append("\tjobjectArray arr = env->NewObjectArray(sym->length,cls,0);\n")
            out.append("\tfor(int i=0; i<sym->length; i++) {\n")
            //TODO: Move the null check to other place? (e.g. struct type transfer and delitearray primitive type transfer)
            if (cppMemMgr == "refcnt" && !isPrimitiveType(typeArg))
              out.append("\t\tjobject obj = (sym->data[i].use_count()==0) ? NULL : sendCPPtoJVM_%s(env, sym->data[i]);\n".format(mangledName(remapHost(typeArg))))
            else
              out.append("\t\tjobject obj = (sym->data[i]==%s) ? NULL : sendCPPtoJVM_%s(env, sym->data[i]);\n".format(nullValue(typeArg),mangledName(remapHost(typeArg))))
            out.append("\t\tenv->SetObjectArrayElement(arr,i,obj);\n")
            out.append("\t\tenv->DeleteLocalRef(obj);\n")
            out.append("\t}\n")
            out.append("\treturn arr;\n")
          }
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure.getSimpleName == "DeliteIndex") {
        val out = new StringBuilder
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        val tp_key = tp.typeArguments(0)
        out.append("\t%s *keys = sym->unsafeKeys();\n".format(remapWithRef(tp_key)))
        out.append("\tjobject m = makeManifest_%s(env);\n".format(mangledName(remapHost(tp_key))))
        if (isPurePrimitiveType(tp_key))
          out.append("\tjclass cls = env->FindClass(\"generated/scala/container/HashMapImpl$mc%s$sp\");\n".format(JNITypeDescriptor(tp_key)))
        else
          out.append("\tjclass cls = env->FindClass(\"generated/scala/container/HashMapImpl\");\n")
        out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IILscala/reflect/Manifest;)V\");\n")
        out.append("\tjobject obj = env->NewObject(cls,mid,sym->indsz(),sym->datasz(),m);\n")
        out.append("\tfor(int i=0; i<sym->size(); i++) {\n")
        out.append("\t\t%s jkey = sendCPPtoJVM_%s(env,keys[i]);\n".format(JNIType(tp_key),mangledName(remapHost(tp_key))))
        out.append("\t\tjmethodID mid_put = env->GetMethodID(cls,\"put\",\"(%s)I\");\n".format(JNITypeDescriptor(tp_key)))
        out.append("\t\tenv->CallIntMethod(obj,mid_put,jkey);\n")
        if (!isPurePrimitiveType(tp_key)) out.append("\t\tenv->DeleteLocalRef(jkey);\n")
        out.append("\t}\n")
        out.append("\treturn obj;\n")
        out.append("\t}\n")
        (signature+";\n", out.toString)
      }
      else 
        super.emitSend(tp, peer)
    case _ => super.emitSend(tp, peer)
  }

  override def emitRecv(tp: Manifest[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPurePrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val typename = if (cppMemMgr == "refcnt") wrapSharedPtr(hostTarget+"Ref"+unwrapSharedPtr(remapHost(typeArg)))
                       else hostTarget+"Ref"+remapHost(typeArg)
        val signature = "%s %srecvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(typename,addRef(),mangledName(typename))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        if (cppMemMgr == "refcnt")
          out.append("\t%s sym(new %s(env->Call%sMethod(obj,mid_get)));\n".format(typename,unwrapSharedPtr(typename),remapToJNI(typeArg)))
        else
          out.append("\t%s %ssym = new %s(env->Call%sMethod(obj,mid_get));\n".format(typename,addRef(),typename,remapToJNI(typeArg)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "%s %srecvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),addRef(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        if (cppMemMgr == "refcnt")
          out.append("\t%s sym(new %s(),%sD());\n".format(remapHost(tp),unwrapSharedPtr(remapHost(tp)),unwrapSharedPtr(remapHost(tp))))
        else
          out.append("\t%s %ssym = new %s();\n".format(remapHost(tp),addRef(tp),remapHost(tp)))
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            if(Config.generateSerializable) {
              out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()Lppl/delite/runtime/data/DeliteArray%s;\");\n".format(elem._1,elem._1,elemtp.typeArguments.head))
              out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format(JNIType(elemtp),elem._1,remapToJNI(elemtp),elem._1))
              out.append("\t%s %s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),elem._1,remapHost(elemtp),elem._1))
            }
            else {
              out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp)))
              out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format(JNIType(elemtp),elem._1,remapToJNI(elemtp),elem._1))
              out.append("\t%s %s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),elem._1,remapHost(elemtp),elem._1))
            }
          }
          else if (encounteredStructs.contains(structName(elemtp))) { 
            out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()Lgenerated/scala/%s;\");\n".format(elem._1,elem._1,structName(elemtp)))
            out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format("jobject",elem._1,"Object",elem._1))
            out.append("\t%s %s%s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),addRef(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
          }
          else {
            if(Config.generateSerializable) {
              out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()Lppl/delite/runtime/data/DeliteArrayObject;\");\n".format(elem._1,elem._1))           
              out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format("jobject",elem._1,"Object",elem._1))
              out.append("\t%s %s%s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),addRef(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
            }
            else {
              out.append("\tjmethodID mid_get_%s = env->GetMethodID(cls,\"%s\",\"()[%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp.typeArguments.head)))
              out.append("\t%s j_%s = env->Call%sMethod(obj,mid_get_%s);\n".format("jobject",elem._1,"Object",elem._1))
              out.append("\t%s %s%s = recvCPPfromJVM_%s(env,j_%s);\n".format(remapHost(elemtp),addRef(elemtp),elem._1,mangledName(remapHost(elemtp)),elem._1))
            }
          }
          out.append("\tsym->%s = %s;\n".format(elem._1,elem._1))
        }
        for(elem <- encounteredStructs(structName(tp)) if !isPurePrimitiveType(baseType(elem._2))) 
          out.append("\tenv->DeleteLocalRef(j_" + elem._1 + ");\n")
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "%s %srecvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),addRef(tp),mangledName(remapHost(tp)))  
        out.append(signature + " {\n")
        if(isPurePrimitiveType(typeArg)) {
          if(Config.generateSerializable) {
            out.append("\tjclass cls = env->FindClass(\"ppl/delite/runtime/data/LocalDeliteArray%s\");\n".format(remapToJNI(typeArg)))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tjobject arr = env->CallObjectMethod(obj,mid);\n")
            out.append("\tint length = env->GetArrayLength((%sArray)arr);\n".format(JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)arr,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            out.append("\tHost%s *sym = new Host%s(length);\n".format(remapHost(tp),remapHost(tp)))
            out.append("\tmemcpy(sym->data, dataPtr, length*sizeof(%s));\n".format(remapHost(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)arr, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("\tenv->DeleteLocalRef(arr);\n")
            out.append("\treturn sym;\n")
          }
          else {
            out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
            if (cppMemMgr == "refcnt")
              out.append("\t%s sym(new %s(length),%sD());\n".format(remapHost(tp),unwrapSharedPtr(remapHost(tp)),unwrapSharedPtr(remapHost(tp))))
            else
              out.append("\t%s %ssym = new %s(length);\n".format(remapHost(tp),addRef(tp),remapHost(tp)))
            out.append("\tmemcpy(sym->data, dataPtr, length*sizeof(%s));\n".format(remapHost(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
            out.append("\treturn sym;\n")
          }
        }
        else {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          if (cppMemMgr == "refcnt")
            out.append("\t%s sym(new %s(length),%sD());\n".format(remapHost(tp),unwrapSharedPtr(remapHost(tp)),unwrapSharedPtr(remapHost(tp))))
          else
            out.append("\t%s %ssym = new %s(length);\n".format(remapHost(tp),addRef(tp),remapHost(tp)))
          out.append("\tfor(int i=0; i<length; i++) {\n")
          //TODO: Move the null check to other place? (e.g. struct type transfer and delitearray primitive type transfer)
          out.append("\t\tjobject o = env->GetObjectArrayElement((%sArray)obj,i);\n".format(JNIType(typeArg)))
          out.append("\t\tsym->data[i] = (o == NULL)? %s : recvCPPfromJVM_%s(env, o);\n".format(nullValue(typeArg),mangledName(remapHost(typeArg))))
          out.append("\t\tenv->DeleteLocalRef(o);\n")
          out.append("\t}\n")
          out.append("\treturn sym;\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure.getSimpleName == "DeliteIndex") {
        val out = new StringBuilder
        val signature = "%s %srecvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),addRef(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
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
  override def emitSendView(tp: Manifest[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPurePrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val typename = if (cppMemMgr == "refcnt") wrapSharedPtr(hostTarget+"Ref"+unwrapSharedPtr(remapHost(typeArg)))
                       else hostTarget+"Ref"+remapHost(typeArg)
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(typename),typename,addRef())
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")  
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure.getSimpleName == "DeliteIndex") {
        val out = new StringBuilder
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitSendView(tp, peer)  
    }
    else
      super.emitSendView(tp, peer)
  }

  override def emitRecvView(tp: Manifest[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPurePrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val typename = if (cppMemMgr == "refcnt") wrapSharedPtr(hostTarget+"Ref"+unwrapSharedPtr(remapHost(typeArg)))
                       else hostTarget+"Ref"+remapHost(typeArg)
        val signature = "%s %srecvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(typename,addRef(),mangledName(typename))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        if (cppMemMgr == "refcnt")
          out.append("\t%s sym(new %s(env->Call%sMethod(obj,mid_get)));\n".format(typename,unwrapSharedPtr(typename),remapToJNI(typeArg)))
        else
          out.append("\t%s %ssym = new %s(env->Call%sMethod(obj,mid_get));\n".format(typename,addRef(),typename,remapToJNI(typeArg)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        //val typeArg = tp.typeArguments.head
        val signature = "%s %srecvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),addRef(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "%s %srecvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),addRef(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        if (isPurePrimitiveType(typeArg)) {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          if (cppMemMgr == "refcnt")
            out.append("\t%s sym(new %s((%s *)dataPtr,length),%sD());\n".format(remapHost(tp),unwrapSharedPtr(remapHost(tp)),remapHost(typeArg),unwrapSharedPtr(remapHost(tp))))
          else
            out.append("\t%s %ssym = new %s((%s *)dataPtr,length);\n".format(remapHost(tp),addRef(tp),remapHost(tp),remapHost(typeArg)))
          out.append("\treturn sym;\n")  
        } 
        else {
          out.append("\tassert(false);\n")
        }    
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure.getSimpleName == "DeliteIndex") {
        val out = new StringBuilder
        val signature = "%s %srecvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),addRef(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitRecvView(tp, peer)  
    }
    else
      super.emitRecvView(tp, peer)
  }


  override def emitSendUpdate(tp: Manifest[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPurePrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val typename = if (cppMemMgr == "refcnt") wrapSharedPtr(hostTarget+"Ref"+unwrapSharedPtr(remapHost(typeArg)))
                       else hostTarget+"Ref"+remapHost(typeArg)
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s %ssym)".format(mangledName(typename),typename,addRef())
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        out.append("\tjmethodID mid_set = env->GetMethodID(cls,\"set$mc%s$sp\",\"(%s)V\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
        out.append("\tenv->CallVoidMethod(obj,mid_set,sym->get());\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if(encounteredStructs.contains(structName(tp))) {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject &obj, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        var args = ""
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s_$eq\",\"(%s)V\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp)))
            out.append("\t%s obj_%s = sendCPPtoJVM_%s(env,sym->%s);\n".format(JNIType(elemtp), elem._1,mangledName(remapHost(elemtp)),elem._1))
            out.append("\tenv->CallVoidMethod(obj,mid_%s,obj_%s);\n".format(elem._1,elem._1))
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
        for(elem <- encounteredStructs(structName(tp)) if !isPurePrimitiveType(baseType(elem._2))) 
          out.append("\tenv->DeleteLocalRef(obj_" + elem._1 + ");\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject &obj, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        if (isPurePrimitiveType(typeArg)) {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          //this check is needed to create a new array if the size has been changed (same for the emitRecvUpdate implementation below)
          //(e.g., this was nested array of a struct and mutation changed the array object)
          out.append("\tif(length != sym->length) {\n")
          out.append("\t\t%sArray arr = env->New%sArray(sym->length);\n".format(JNIType(typeArg),remapToJNI(typeArg)))
          out.append("\t\tenv->DeleteLocalRef(obj);\n") // Is it safe to release here?
          out.append("\t\tobj = (jobject)arr;\n")
          out.append("\t}\n")
          out.append("\t\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\tmemcpy(dataPtr, sym->data, sym->length*sizeof(%s));\n".format(remapHost(typeArg)))
          out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
        }
        else {
          out.append("\tassert(false);\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure.getSimpleName == "DeliteIndex") {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject &obj, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else
        super.emitSendUpdate(tp, peer)  
    }
    else
      super.emitSendUpdate(tp, peer)
  }

  override def emitRecvUpdate(tp: Manifest[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (tp.erasure == classOf[Variable[AnyVal]]) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        if (!isPurePrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
        val typename = if (cppMemMgr == "refcnt") wrapSharedPtr(hostTarget+"Ref"+unwrapSharedPtr(remapHost(typeArg)))
                       else hostTarget+"Ref"+remapHost(typeArg)
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s %ssym)".format(mangledName(typename),typename,addRef())
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
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("\tjclass cls = env->GetObjectClass(obj);\n")
        var args = ""
        for(elem <- encounteredStructs(structName(tp))) {
          val elemtp = baseType(elem._2)
          if(isPrimitiveType(elemtp)) {
            out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()%s\");\n".format(elem._1,elem._1,JNITypeDescriptor(elemtp)))
            out.append("\t%s obj_%s = env->Call%sMethod(obj, mid_%s);\n".format(JNIType(elemtp),elem._1,remapToJNI(elemtp),elem._1))
            out.append("\tsym->%s = recvCPPfromJVM_%s(env,obj_%s);\n".format(elem._1,mangledName(remapHost(elemtp)),elem._1))
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
        for(elem <- encounteredStructs(structName(tp)) if !isPurePrimitiveType(baseType(elem._2))) 
          out.append("\tenv->DeleteLocalRef(obj_" + elem._1 + ");\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isArrayType(tp)) {
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        if (isPurePrimitiveType(typeArg)) {
          out.append("\tint length = env->GetArrayLength((%sArray)obj);\n".format(JNIType(typeArg)))
          out.append("\tif(length != sym->length)\n")
          out.append("\t\tsym->data = (%s*)realloc((void*)(sym->data),sizeof(%s)*length);\n".format(remapHost(typeArg),remapHost(typeArg)))
          out.append("\tsym->length = length;\n")
          out.append("\t%s *dataPtr = (%s*)env->GetPrimitiveArrayCritical((%sArray)obj,0);\n".format(JNIType(typeArg),JNIType(typeArg),JNIType(typeArg)))
          out.append("\tmemcpy(sym->data, dataPtr, length*sizeof(%s));\n".format(remapHost(typeArg)))
          out.append("\tenv->ReleasePrimitiveArrayCritical((%sArray)obj, dataPtr, 0);\n".format(JNIType(typeArg)))
        }
        else {
          out.append("\tassert(false);\n")
        }
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure.getSimpleName == "DeliteIndex") {
        val out = new StringBuilder
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s %ssym)".format(mangledName(remapHost(tp)),remapHost(tp),addRef(tp))
        out.append(signature + " {\n")
        out.append("assert(false);\n")
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
