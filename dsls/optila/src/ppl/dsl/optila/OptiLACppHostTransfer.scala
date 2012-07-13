package ppl.dsl.optila

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer}

trait OptiLACppHostTransfer extends CppHostTransfer {

  val IR: Expressions
  import IR._

  override def emitSend(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector<bool>" | "DenseVector<char>" | "DenseVector<CHAR>" | "DenseVector<short>" | "DenseVector<int>" | "DenseVector<long>" | "DenseVector<float>" | "DenseVector<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseVector\");\n".format(typeArg.toString))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
            out.append("\tjobject obj = env->NewObject(cls,mid,%s->length,%s->isRow);\n".format(quote(sym),quote(sym)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn obj;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix<bool>" | "DenseMatrix<char>" | "DenseMatrix<CHAR>" | "DenseMatrix<short>" | "DenseMatrix<int>" | "DenseMatrix<long>" | "DenseMatrix<float>" | "DenseMatrix<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseVector\");\n".format(typeArg.toString))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
            out.append("\tjobject obj = env->NewObject(cls,mid,%s->numRows,%s->numCols);\n".format(quote(sym),quote(sym)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn obj;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSend(sym, host)
        }
    }
    else
      super.emitSend(sym, host)
  }


  override def emitRecv(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector<bool>" | "DenseVector<char>" | "DenseVector<CHAR>" | "DenseVector<short>" | "DenseVector<int>" | "DenseVector<long>" | "DenseVector<float>" | "DenseVector<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
            out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix<bool>" | "DenseMatrix<char>" | "DenseMatrix<CHAR>" | "DenseMatrix<short>" | "DenseMatrix<int>" | "DenseMatrix<long>" | "DenseMatrix<float>" | "DenseMatrix<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_numRows),env->CallBooleanMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecv(sym, host)
        }
    }
    else
      super.emitRecv(sym, host)
  }

  override def emitSendView(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector<bool>" | "DenseVector<char>" | "DenseVector<CHAR>" | "DenseVector<short>" | "DenseVector<int>" | "DenseVector<long>" | "DenseVector<float>" | "DenseVector<double>" |
               "DenseMatrix<bool>" | "DenseMatrix<char>" | "DenseMatrix<CHAR>" | "DenseMatrix<short>" | "DenseMatrix<int>" | "DenseMatrix<long>" | "DenseMatrix<float>" | "DenseMatrix<double>" =>
            val out = new StringBuilder
            val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tassert(false);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendView(sym, host)
        }
    }
    else
      super.emitSendView(sym, host)
  }


  override def emitRecvView(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector<bool>" | "DenseVector<char>" | "DenseVector<CHAR>" | "DenseVector<short>" | "DenseVector<int>" | "DenseVector<long>" | "DenseVector<float>" | "DenseVector<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\t%s *%s = new %s(dataPtr,env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tenv->DeleteLocalRef(data);\n")   //TODO: This should not be done at this point?
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix<bool>" | "DenseMatrix<char>" | "DenseMatrix<CHAR>" | "DenseMatrix<short>" | "DenseMatrix<int>" | "DenseMatrix<long>" | "DenseMatrix<float>" | "DenseMatrix<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvView	CPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\t%s *%s = new %s(dataPtr,env->CallIntMethod(obj,mid_numRows),env->CallBooleanMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tenv->DeleteLocalRef(data);\n")   //TODO: This should not be done at this point?
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecvView(sym, host)
        }
    }
    else
      super.emitRecvView(sym, host)
  }

  override def emitSendUpdate(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector<bool>" | "DenseVector<char>" | "DenseVector<CHAR>" | "DenseVector<short>" | "DenseVector<int>" | "DenseVector<long>" | "DenseVector<float>" | "DenseVector<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix<bool>" | "DenseMatrix<char>" | "DenseMatrix<CHAR>" | "DenseMatrix<short>" | "DenseMatrix<int>" | "DenseMatrix<long>" | "DenseMatrix<float>" | "DenseMatrix<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(sym, host)
        }
    }
    else
      super.emitSendUpdate(sym, host)
  }

  override def emitRecvUpdate(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector<bool>" | "DenseVector<char>" | "DenseVector<CHAR>" | "DenseVector<short>" | "DenseVector<int>" | "DenseVector<long>" | "DenseVector<float>" | "DenseVector<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix<bool>" | "DenseMatrix<char>" | "DenseMatrix<CHAR>" | "DenseMatrix<short>" | "DenseMatrix<int>" | "DenseMatrix<long>" | "DenseMatrix<float>" | "DenseMatrix<double>" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(sym, host)
        }
    }
    else
      super.emitRecvUpdate(sym, host)
  }

}