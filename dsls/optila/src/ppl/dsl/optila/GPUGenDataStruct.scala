package ppl.dsl.optila

import virtualization.lms.internal.{OpenCLCodegen, GPUCodegen, Expressions, CudaCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait GPUGenDataStruct extends GPUCodegen {

  val IR: Expressions
  import IR._

  val VectorImplCls = "jclass VectorImplCls = env->FindClass(\"generated/scala/VectorImpl\");\n"
  val VectorViewImplCls = "jclass VectorViewImplCls = env->FindClass(\"generated/scala/VectorViewImpl\");\n"
  val RangeVectorImplCls = "jclass RangeVectorImplCls = env->FindClass(\"generated/scala/RangeVectorImpl\");\n"

  val MatrixImplCls = "jclass MatrixImplCls = env->FindClass(\"generated/scala/MatrixImpl\");\n"

  def densevectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
    out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))

    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))

    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))

    out.append("\tmemcpy(hostPtr, dataPtr, %s);\n".format(numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s->data, hostPtr, %s);\n".format(quote(sym),numBytesStr))

    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def rangevectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")
    out.append("\tjmethodID mid_start = env->GetMethodID(cls,\"start\",\"()I\");\n")
    out.append("\tjmethodID mid_stride = env->GetMethodID(cls,\"stride\",\"()I\");\n")
    out.append("\tjmethodID mid_end = env->GetMethodID(cls,\"end\",\"()I\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    out.append("\t%s->isRow = env->CallBooleanMethod(obj,mid_isRow);\n".format(quote(sym)))
    out.append("\t%s->start = env->CallIntMethod(obj,mid_start);\n".format(quote(sym)))
    out.append("\t%s->stride = env->CallIntMethod(obj,mid_stride);\n".format(quote(sym)))
    out.append("\t%s->end = env->CallIntMethod(obj,mid_end);\n".format(quote(sym)))
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))

    out.toString
  }

  def densematrixCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(typeArg))

    // Get class, method ID and set the fields other than data
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
    out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_numRows),env->CallIntMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))

    // Get data(array) from scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))

    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    out.append("\tmemcpy(hostPtr, dataPtr, %s);\n".format(numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s->data, hostPtr, %s);\n".format(quote(sym),numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString

  }

  def delitearrayCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))

    out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remap(typeArg),remap(typeArg),remap(typeArg)))
    out.append("\t%s *%s = new %s(length);\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))

    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))

    out.append("\tmemcpy(hostPtr, dataPtr, %s);\n".format(numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s->data, hostPtr, %s);\n".format(quote(sym),numBytesStr))

    out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remap(typeArg)))
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def densevectorCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseVector\");\n".format(typeArg.toString))
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.length,%s.isRow);\n".format(quote(sym),quote(sym)))

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tmemcpy(dataPtr, hostPtr, %s);\n".format(numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn obj;\n")
    out.toString
  }

  def densematrixCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(typeArg))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseMatrix\");\n".format(typeArg.toString))
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.numRows,%s.numCols);\n".format(quote(sym),quote(sym)))

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tmemcpy(dataPtr, hostPtr, %s);\n".format(numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn obj;\n")
    out.toString
  }

  def delitearrayCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tj%sArray arr = env->New%sArray(%s.length);\n".format(remap(typeArg),remap(typeArg),quote(sym)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)arr,0);\n".format(remap(typeArg),remap(typeArg),remap(typeArg)))

    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))

    out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s->data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tmemcpy(dataPtr, hostPtr, %s);\n".format(numBytesStr))

    out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)arr, dataPtr, 0);\n".format(remap(typeArg)))
    out.append("\treturn arr;\n")
    out.toString
  }

  /*****************************************************************************************
   * transfer functions up to this point are verified
   */

  def densematrixCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))
    out.append("\t%s *devPtr = %s.data;\n".format(remap(typeArg),quote(sym)))
    out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")

    out.toString
  }

  def densevectorCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(remap(typeArg),remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(remap(typeArg),remap(typeArg)))
    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))
    out.append("\t%s *devPtr = %s.data;\n".format(remap(typeArg),quote(sym)))
    out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")

    out.toString
  }

  def delitearrayCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.tp.typeArguments.head
    val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))

    out.append("\tint length = %s.length;\n".format(quote(sym)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remap(typeArg),remap(typeArg),remap(typeArg)))
    out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
    out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))
    out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tmemcpy(dataPtr, hostPtr, %s);\n".format(numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remap(typeArg)))
    out.toString
  }


}


trait CudaGenDataStruct extends CudaCodegen with GPUGenDataStruct {

}

trait OpenCLGenDataStruct extends OpenCLCodegen with GPUGenDataStruct {

}

