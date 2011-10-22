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

  def vectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t\t%s *hostPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t\t%s *devPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t%s->data = devPtr;\n".format(quote(sym)))

    out.append(RangeVectorImplCls)

      // If RangeVector
      out.append("\tif(env->IsInstanceOf(obj,RangeVectorImplCls)) {\n")
      out.append("\t\tint start = env->CallIntMethod(obj,env->GetMethodID(cls,\"start\",\"()I\"));\n")
      out.append("\t\tint end = env->CallIntMethod(obj,env->GetMethodID(cls,\"end\",\"()I\"));\n")
      out.append("\t\tint stride = env->CallIntMethod(obj,env->GetMethodID(cls,\"stride\",\"()I\"));\n")
      out.append("\t\tfor(int i=0; i<%s->length; i++) { hostPtr[i] = start + i * stride; }\n".format(quote(sym)))
      out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
      out.append("\t}\n")

      out.append("\telse {\n")
      out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data$mc%s$sp\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
      out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
      out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
      out.append("\t\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
      out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
      out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
      out.append("\t\tenv->DeleteLocalRef(data);\n")
      out.append("\t}\n")

      out.append("\tenv->DeleteLocalRef(cls);\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.toString
    }

  def matrixCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Get class, method ID and set the fields other than data
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")
    out.append("\t%s->numRows = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    out.append("\t%s->numCols = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))

    // Get data(array) from scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))

    // Allocate pinned-memory and device memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))

    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))

    // Store the device pointer to the C data structure
    out.append("\t%s->data = %s;\n".format(quote(sym),"devPtr"))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString

  }

  def rangeVectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    //out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")
    out.append("\tjmethodID mid_start = env->GetMethodID(cls,\"start\",\"()I\");\n")
    out.append("\tjmethodID mid_stride = env->GetMethodID(cls,\"stride\",\"()I\");\n")
    out.append("\tjmethodID mid_end = env->GetMethodID(cls,\"end\",\"()I\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t%s->start = env->CallIntMethod(obj,mid_start);\n".format(quote(sym)))
    out.append("\t%s->stride = env->CallIntMethod(obj,mid_stride);\n".format(quote(sym)))
    out.append("\t%s->end = env->CallIntMethod(obj,mid_end);\n".format(quote(sym)))
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))

    out.toString
  }
  
  def matrixCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sMatrixImpl\");\n".format(sym.Type.typeArguments(0)))
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.numRows,%s.numCols);\n".format(quote(sym),quote(sym)))
    out.append("\tif(obj==NULL) std::cout << \"new object NOT created\" << std::endl;\n")

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tif(mid_data==NULL) std::cout << \"data access method NOT found\" << std::endl;\n")
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tif(dataPtr==NULL) std::cout << \"GetPrimitiveArrayCritical call failed\" << std::endl;\n")

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn obj;\n")
    out.toString
  }

  def matrixCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr = %s.data;\n".format(typeStr,quote(sym)))
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")

    out.toString
  }

  def vectorCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sVectorImpl\");\n".format(sym.Type.typeArguments(0)))
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.length,%s.isRow);\n".format(quote(sym),quote(sym)))
    out.append("\tif(obj==NULL) std::cout << \"new object NOT created\" << std::endl;\n")

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tif(mid_data==NULL) std::cout << \"data access method NOT found\" << std::endl;\n")
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tif(dataPtr==NULL) std::cout << \"GetPrimitiveArrayCritical call FAILED\" << std::endl;\n")

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  def vectorCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr = %s.data;\n".format(typeStr,quote(sym)))
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")

    out.toString
  }

  def rangeVectorCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    out.append("\tstd::cout << \"CopyDtoHBack rangeVector is not supported yet\" << std::endl;\n")
    out.toString
  }


  // Dummy methods temporarily just for the compilation
  //def emitVectorAlloc(newSym:Sym[_],length:String,isRow:String,reset:Boolean,data:String=null) {}
  def emitVectorAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  def emitVectorAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}
  //def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null) {}
  def emitMatrixAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  def emitMatrixAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}

  // Generate & register temporary data structures (which could be the output) for GPU kernel
  def emitVectorAlloc(newSym:Sym[_], length:String, isRow:String, reset:Boolean, data:String=null):Unit = {
    //TODO: Check if both symbols are Vectors

    //Do not add the same temporary if it already exists
    if(getKernelTemps contains newSym) return

    helperFuncIdx += 1

    val out = new StringBuilder

    val args = (getKernelOutputs ::: getKernelInputs ::: getKernelTemps) filterNot (_==newSym)

    out.append("\t%s *%s = new %s();\n".format(remap(newSym.Type),quote(newSym),remap(newSym.Type)))

    //val mult = if(currDim==2) xDimList(0) else "1"
    //if(currDim==2) multDimInputs += newSym

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
    out.append("\treturn %s;\n".format(quote(newSym)))

    val allocStr = emitAllocOutput(newSym, null, out.toString, args)
    helperFuncString.append(allocStr)

    val copyStr = emitCopyOutputDtoH(newSym, null, copyOutputDtoH(newSym))
    helperFuncString.append(copyStr)
  }

  def vectorPositionMultDimInputs(sym: Sym[Any]) : String = {
    val out = new StringBuilder
    currDim = 1
    val currDimStr = getCurrDimStr()
    out.append("\t%s.data += %s * %s.length;\n".format(quote(sym),currDimStr,quote(sym)))
    out.toString
  }

  def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null): Unit = {
    //TODO: Check if both symbols are Matrices

    //Do not add the same temporary if it already exists
    if(getKernelTemps contains newSym) return

    helperFuncIdx += 1

    val out = new StringBuilder
    val args = (getKernelOutputs ::: getKernelInputs ::: getKernelTemps) filterNot (_==newSym)

    out.append("\t%s *%s = new %s();\n".format(remap(newSym.Type),quote(newSym),remap(newSym.Type)))

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
  /*
  def emitMatrixAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false): Unit = {
    emitMatrixAlloc(newSym, quote(sym)+"->numRows", quote(sym)+"->numCols",reset)
  }

  def emitMatrixAllocRef(newSym:Sym[Any], sym:Sym[Any]): Unit = {
    // Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

	  helperFuncIdx += 1

    val out = new StringBuilder
    val paramStr = if(isObjectType(sym.Type)) remap(sym.Type) + " *" + quote(sym)
				   else remap(sym.Type) + " " + quote(sym)
    val argStr = "\""+quote(sym)+"\""

    out.append("%s *gpuMemAlloc_%s_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,paramStr))
    out.append("\t%s *%s = %s;\n".format(remap(newSym.Type),quote(newSym),quote(sym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s_%s\",[%s],\"gpuMemCopy_%s_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,argStr,quote(kernelSymbol), quote(newSym),helperFuncIdx,"env", quote(newSym))
      out.append(emitCopyOutputDtoH(newSym))
	    gpuOutputs = gpuOutputs :+ newSym
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,argStr))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
  }
  */

  /*
  def emitVectorAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false): Unit = {
    emitVectorAlloc(newSym, quote(sym)+"->length", quote(sym)+"->isRow", reset)
  }

  def emitVectorAllocRef(newSym:Sym[Any], sym:Sym[Any]): Unit = {

    // Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

	helperFuncIdx += 1

    val out = new StringBuilder
    val paramStr = if(isObjectType(sym.Type)) remap(sym.Type) + " *" + quote(sym)
				   else remap(sym.Type) + " " + quote(sym)
    val argStr = "\""+quote(sym)+"\""

    out.append("%s *gpuMemAlloc_%s_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,paramStr))
    out.append("\t%s *%s = %s;\n".format(remap(newSym.Type),quote(newSym),quote(sym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s_%s\",[%s],\"gpuMemCopy_%s_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,argStr,quote(kernelSymbol), quote(newSym),helperFuncIdx,"env", quote(newSym))
      out.append(emitCopyOutputDtoH(newSym))
	    gpuOutputs = gpuOutputs :+ newSym
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,argStr))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
  }
  */
}

