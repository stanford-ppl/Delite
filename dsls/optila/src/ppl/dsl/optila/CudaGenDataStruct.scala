package ppl.dsl.optila

import _root_.scala.virtualization.lms.internal.{Expressions, CudaCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait CudaGenDataStruct extends CudaCodegen {

  val IR: Expressions
  import IR._

  val VectorImplCls = "jclass VectorImplCls = env->FindClass(\"generated/scala/VectorImpl\");\n"
  val VectorViewImplCls = "jclass VectorViewImplCls = env->FindClass(\"generated/scala/VectorViewImpl\");\n"
  val RangeVectorImplCls = "jclass RangeVectorImplCls = env->FindClass(\"generated/scala/RangeVectorImpl\");\n"

  val MatrixImplCls = "jclass MatrixImplCls = env->FindClass(\"generated/scala/MatrixImpl\");\n"

  def densevectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = sym.Type.typeArguments.head
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
    out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))

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

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
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
    val typeArg = sym.Type.typeArguments.head
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(typeArg))

    // Get class, method ID and set the fields other than data
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
    out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_numRows),env->CallIntMethod(obj,mid_numCols));\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))

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
    val typeArg = sym.Type.typeArguments.head
    val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))

    out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remap(typeArg)))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remap(typeArg),remap(typeArg),remap(typeArg)))
    out.append("\t%s *%s = new %s(length);\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))

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
    val typeArg = sym.Type.typeArguments.head
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
    val typeArg = sym.Type.typeArguments.head
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
    val typeArg = sym.Type.typeArguments.head
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
    val typeArg = sym.Type.typeArguments.head
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
    val typeArg = sym.Type.typeArguments.head
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
    val typeArg = sym.Type.typeArguments.head
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

  // Dummy methods temporarily just for the compilation
  //def emitVectorAlloc(newSym:Sym[_],length:String,isRow:String,reset:Boolean,data:String=null) {}
  //def emitVectorAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  //def emitVectorAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}
  //def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null) {}
  //def emitMatrixAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  //def emitMatrixAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}

  /*
  // Generate & register temporary data structures (which could be the output) for GPU kernel
  def emitVectorAlloc(newSym:Sym[_], length:String, isRow:String, reset:Boolean, data:String=null):Unit = {
    //TODO: Check if both symbols are Vectors

    //Do not add the same temporary if it already exists
    if(getKernelTemps contains newSym) return

    helperFuncIdx += 1

    val out = new StringBuilder
    val args = (getKernelOutputs ::: getKernelInputs ::: getKernelTemps) filterNot (_==newSym)

    out.append("\t%s *%s = new %s(%s,%s);\n".format(remap(newSym.Type),quote(newSym),remap(newSym.Type),length,isRow))

    /*
    // Check if new allocation is needed
    if(data==null) {
      out.append("\t%s *devPtr;\n".format(remap(newSym.Type.typeArguments(0))))
      out.append("\tDeliteCudaMalloc((void**)%s,%s*sizeof(%s));\n".format("&devPtr",length,remap(newSym.Type.typeArguments(0))))
      if(reset) out.append("\tDeliteCudaMemset(devPtr,0,%s*sizeof(%s));\n".format(length,remap(newSym.Type.typeArguments(0))))
      out.append("\t%s->length = %s;\n".format(quote(newSym),length))
      out.append("\t%s->isRow = %s;\n".format(quote(newSym),isRow))
      out.append("\t%s->data = devPtr;\n".format(quote(newSym)))
    }
    else {
      out.append("\t%s->length = %s;\n".format(quote(newSym),length))
      out.append("\t%s->isRow = %s;\n".format(quote(newSym),isRow))
      out.append("\t%s->data = %s;\n".format(quote(newSym),data))      
    }
    */
    out.append("\treturn %s;\n".format(quote(newSym)))

    val allocStr = emitAllocOutput(newSym, null, out.toString, args)
    helperFuncString.append(allocStr)

    val copyStr = emitCopyOutputDtoH(newSym, null, copyOutputDtoH(newSym))
    helperFuncString.append(copyStr)
  }

  /*
  def vectorPositionMultDimInputs(sym: Sym[Any]) : String = {
    val out = new StringBuilder
    //currDim = 1
    //val currDimStr = getCurrDimStr()
    out.append("\t%s.data += %s * %s.length;\n".format(quote(sym),currDimStr,quote(sym)))
    out.toString
  }
  */

  def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null): Unit = {
    //TODO: Check if both symbols are Matrices

    //Do not add the same temporary if it already exists
    if(getKernelTemps contains newSym) return

    helperFuncIdx += 1

    val out = new StringBuilder
    val args = (getKernelOutputs ::: getKernelInputs ::: getKernelTemps) filterNot (_==newSym)

    out.append("\t%s *%s = new %s(%s,%s);\n".format(remap(newSym.Type),quote(newSym),remap(newSym.Type),numRows,numCols))

    /*
    // Check if new allocation is needed
    if(data==null) {
      out.append("\t%s *devPtr;\n".format(remap(newSym.Type.typeArguments(0))))
      out.append("\tDeliteCudaMalloc((void**)%s,%s*%s*sizeof(%s));\n".format("&devPtr",numRows,numCols,remap(newSym.Type.typeArguments(0))))
      if(reset) out.append("\tDeliteCudaMemset(devPtr,0,%s*%s*sizeof(%s));\n".format(numRows,numCols,remap(newSym.Type.typeArguments(0))))
      out.append("\t%s->numRows = %s;\n".format(quote(newSym),numRows))
      out.append("\t%s->numCols = %s;\n".format(quote(newSym),numCols))
      out.append("\t%s->data = devPtr;\n".format(quote(newSym)))
    }
    else {
      out.append("\t%s->numRows = %s;\n".format(quote(newSym),numRows))
      out.append("\t%s->numCols = %s;\n".format(quote(newSym),numCols))
      out.append("\t%s->data = %s;\n".format(quote(newSym),data))
    }
    */
    out.append("\treturn %s;\n".format(quote(newSym)))

    val allocStr = emitAllocOutput(newSym, null, out.toString, args)
    helperFuncString.append(allocStr)

    val copyStr = emitCopyOutputDtoH(newSym, null, copyOutputDtoH(newSym))
    helperFuncString.append(copyStr)

  }

  def vectorClone(sym: Sym[Any], src: Sym[Any]) : String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s_ptr->length * sizeof(%s)".format(quote(src),remap(typeArg))

    out.append("//calling vectorClone\n")
    out.append("%s *%s_ptr = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("%s_ptr->length = %s_ptr->length;\n".format(quote(sym),quote(src)))
    out.append("%s_ptr->isRow = %s_ptr->isRow;\n".format(quote(sym),quote(src)))
    out.append("%s *devPtr;\n".format(typeStr))
    out.append("DeliteCudaMalloc((void**)&devPtr,%s);\n".format(numBytesStr))
    out.append("%s_ptr->data = devPtr;\n".format(quote(sym)))
    out.append("DeliteCudaMemcpyDtoDAsync(%s_ptr->data,%s_ptr->data,%s);\n".format(quote(sym),quote(src),numBytesStr))

    out.toString
  }

  def matrixClone(sym: Sym[Any], src: Sym[Any]) : String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s_ptr->numRows * %s_ptr->numCols * sizeof(%s)".format(quote(src),quote(src),remap(typeArg))

    out.append("//calling matrixClone\n")
    out.append("%s *%s_ptr = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("%s_ptr->numRows = %s_ptr->numRows;\n".format(quote(sym),quote(src)))
    out.append("%s_ptr->numCols = %s_ptr->numCols;\n".format(quote(sym),quote(src)))
    out.append("%s *devPtr;\n".format(typeStr))
    out.append("DeliteCudaMalloc((void**)&devPtr,%s);\n".format(numBytesStr))
    out.append("%s_ptr->data = devPtr;\n".format(quote(sym)))
    out.append("DeliteCudaMemcpyDtoDAsync(%s_ptr->data,%s_ptr->data,%s);\n".format(quote(sym),quote(src),numBytesStr))

    out.toString
  }
  */
}

