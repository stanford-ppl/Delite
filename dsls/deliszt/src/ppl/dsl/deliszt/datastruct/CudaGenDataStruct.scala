package ppl.dsl.deliszt.datastruct

import _root_.scala.virtualization.lms.internal.{Expressions, CudaCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait CudaGenDataStruct extends CudaCodegen {

  val IR: Expressions
  import IR._

  def matCopyInputHtoD(sym: Sym[Any]): String = {
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

  def vecCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")

	out.append("\tjclass viewCls = env->FindClass(\"generated/scala/%sVecViewImpl\");\n".format(typeArg.toString));
	out.append("\tjboolean isViewCls = env->IsInstanceOf(obj,viewCls);\n");
	out.append("\tjclass zeroCls = env->FindClass(\"generated/scala/VecImpl\");\n");
	out.append("\tjboolean isZeroCls = env->IsInstanceOf(obj,zeroCls);\n");

	  // If this is not RangeVec
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    
	out.append("\tif(isZeroCls) {\n")
   	out.append("\t\t%s *hostPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t\t%s *devPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t\tmemset(%s, 0, %s);\n".format("hostPtr",numBytesStr))
    out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    out.append("\t\t%s->data = %s;\n".format(quote(sym),"devPtr"))
	out.append("\t}\n")
	out.append("\telse {\n")


	out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
	
	// Check if vector view
	out.append("\tif(isViewCls) {\n")
	out.append("\t\tint start = 0;\n")
	out.append("\t\tjmethodID mid_start = env->GetMethodID(cls,\"start\",\"()I\");\n")
	out.append("\t\tstart = env->CallIntMethod(obj,mid_start);\n")
	out.append("\t\tdataPtr += start;\n")
	out.append("\t}")

    // Allocate pinned-memory and device memory
    out.append("\t\t%s *hostPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t\t%s *devPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    out.append("\t\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    // Store the device pointer to the C data structure
    out.append("\t\t%s->data = %s;\n".format(quote(sym),"devPtr"))
    // Release
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
	out.append("\t}\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def matCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sMatImpl\");\n".format(sym.Type.typeArguments(0)))
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s->numRows,%s->numCols);\n".format(quote(sym),quote(sym)))
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
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s->data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  def matCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr = %s->data;\n".format(typeStr,quote(sym)))
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s->data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.toString
  }

  def vecCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sVecImpl\");\n".format(sym.Type.typeArguments(0)))
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s->length);\n".format(quote(sym),quote(sym)))
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
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s->data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  def vecCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr = %s->data;\n".format(typeStr,quote(sym)))
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s->data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.toString
  }

 // Dummy methods temporarily just for the compilation
  //def emitVecAlloc(newSym:Sym[_],length:String,reset:Boolean,data:String=null) {}
  def emitVecAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  def emitVecAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}
  //def emitMatAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null) {}
  def emitMatAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  def emitMatAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}

  // Generate & register temporary data structures (which could be the output) for GPU kernel
  def emitVecAlloc(newSym:Sym[_], length:String, reset:Boolean, data:String=null):Unit = {
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
      out.append("\t%s->data = devPtr;\n".format(quote(newSym)))
    }
    else {
      out.append("\t%s->length = %s;\n".format(quote(newSym),length))
      out.append("\t%s->data = %s;\n".format(quote(newSym),data))      
    }
    out.append("\treturn %s;\n".format(quote(newSym)))

    val allocStr = emitAllocOutput(newSym, null, out.toString, args)
    helperFuncString.append(allocStr)

    val copyStr = emitCopyOutputDtoH(newSym, null, copyOutputDtoH(newSym))
    helperFuncString.append(copyStr)
  }

  /*
  def emitVecAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false): Unit = {
    emitVecAlloc(newSym, quote(sym)+"->length", reset)
  }

  def emitVecAllocRef(newSym:Sym[Any], sym:Sym[Any]): Unit = {

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
  def vecPositionMultDimInputs(sym: Sym[Any]) : String = {
    val out = new StringBuilder
    currDim = 1
    val currDimStr = getCurrDimStr()
      out.append("\t%s.data += %s * %s.length;\n".format(quote(sym),currDimStr,quote(sym)))
    out.toString
  }

  def emitMatAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null): Unit = {
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

  /*
  def emitMatAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false): Unit = {
    emitMatAlloc(newSym, quote(sym)+"->numRows", quote(sym)+"->numCols",reset)
  }

  def emitMatAllocRef(newSym:Sym[Any], sym:Sym[Any]): Unit = {
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

