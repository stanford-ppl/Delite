package ppl.dsl.optila

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer, CLikeCodegen}

trait OptiLACppHostTransfer extends CppHostTransfer {
  this: CLikeCodegen =>

  val IR: Expressions
  import IR._

  override def emitSend(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseVector\");\n".format(typeArg.toString))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
            out.append("\tjobject obj = env->NewObject(cls,mid,%s->length,%s->isRow);\n".format("sym","sym"))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn obj;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseMatrix\");\n".format(typeArg.toString))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
            out.append("\tjobject obj = env->NewObject(cls,mid,%s->numRows,%s->numCols);\n".format("sym","sym"))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn obj;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSend(tp, host)
        }
    }
    else
      super.emitSend(tp, host)
  }


  override def emitRecv(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "Host%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
            out.append("\tHost%s *%s = new Host%s(env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(tp),"sym",remap(tp)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format("sym"))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "Host%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\tHost%s *%s = new Host%s(env->CallIntMethod(obj,mid_numRows),env->CallIntMethod(obj,mid_numCols));\n".format(remap(tp),"sym",remap(tp)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format("sym"))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecv(tp, host)
        }
    }
    else
      super.emitRecv(tp, host)
  }

  override def emitSendView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" |
          "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tassert(false);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendView(tp, host)
        }
    }
    else
      super.emitSendView(tp, host)
  }


  override def emitRecvView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "Host%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tHost%s *%s = new Host%s((%s *)dataPtr,env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(tp),"sym",remap(tp),remap(typeArg)))
            out.append("\tenv->DeleteLocalRef(data);\n")   //TODO: This should not be done at this point?
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format("sym"))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "Host%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tHost%s *%s = new Host%s((%s *)dataPtr,env->CallIntMethod(obj,mid_numRows),env->CallIntMethod(obj,mid_numCols));\n".format(remap(tp),"sym",remap(tp),remap(typeArg)))
            out.append("\tenv->DeleteLocalRef(data);\n")   //TODO: This should not be done at this point?
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format("sym"))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecvView(tp, host)
        }
    }
    else
      super.emitRecvView(tp, host)
  }

  override def emitSendUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(tp, host)
        }
    }
    else
      super.emitSendUpdate(tp, host)
  }

  override def emitRecvUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym")
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecvUpdate(tp, host)
        }
    }
    else
      super.emitRecvUpdate(tp, host)
  }

}
