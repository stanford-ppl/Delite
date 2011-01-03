package ppl.dsl.optiml.datastruct

import _root_.scala.virtualization.lms.internal.{Expressions, CudaCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait CudaGenDataStruct extends CudaCodegen {

  val IR: Expressions
  import IR._

  def matrixCopyHtoD(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Get class, method ID and set the fields other than data
    out.append("\t%s %s;\n".format(remap(sym.Type),quote(sym)))
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")
    out.append("\t%s.numRows = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    out.append("\t%s.numCols = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))

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
    out.append("\t%s.data = %s;\n".format(quote(sym),"devPtr"))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString

  }

  def vectorCopyHtoD(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")

	  //out.append("\tjclass rangeCls = env->FindClass(\"generated/scala/RangeVectorImpl\");\n");
	  //out.append("\tjboolean isRangeCls = env->IsInstanceOf(obj,rangeCls);\n");

	  // If this is not RangeVector
	  //out.append("\tif(!isRangeCls) {\n");
    out.append("\t\t%s %s;\n".format(remap(sym.Type),quote(sym)))
    out.append("\t\t%s.length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t\t%s.isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    // Allocate pinned-memory and device memory
    out.append("\t\t%s *hostPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t\t%s *devPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    out.append("\t\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    // Store the device pointer to the C data structure
    out.append("\t\t%s.data = %s;\n".format(quote(sym),"devPtr"))
    // Release
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\treturn %s;\n".format(quote(sym)))
	  //out.append("\t}\n")

	/*
	  // If this is RangeVector 
	  out.append("\telse {\n");
    out.append("\t\t%s %s;\n".format(remap(sym.Type),quote(sym)))
    out.append("\t\t%s.length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t\t%s.isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t\t%s *hostPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t\t%s *devPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t\tjmethodID mid_start = env->GetMethodID(cls,\"start\",\"()I\");\n")
    out.append("\t\tjmethodID mid_stride = env->GetMethodID(cls,\"stride\",\"()I\");\n")
    out.append("\t\tint start = env->CallIntMethod(obj,mid_start);\n")
    out.append("\t\tint stride = env->CallIntMethod(obj,mid_stride);\n")
    out.append("\t\tfor(int i=0; i<%s.length; i++) {\n".format(quote(sym)))
    out.append("\t\t\thostPtr[i] = start + i * stride;\n".format(quote(sym),quote(sym)))
    out.append("\t\t}\n")
    out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    out.append("\t\t%s.data = %s;\n".format(quote(sym),"devPtr"))
    out.append("\t\treturn %s;\n".format(quote(sym)))
	  out.append("\t}\n")
	*/
    out.toString
  }

  def rangevectorCopyHtoD(sym: Sym[_]): String = {
    val out = new StringBuilder

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")
    out.append("\tjmethodID mid_start = env->GetMethodID(cls,\"start\",\"()I\");\n")
    out.append("\tjmethodID mid_stride = env->GetMethodID(cls,\"stride\",\"()I\");\n")
    out.append("\tjmethodID mid_end = env->GetMethodID(cls,\"end\",\"()I\");\n")

    out.append("\t%s %s;\n".format(remap(sym.Type),quote(sym)))
    out.append("\t%s.length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t%s.isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t%s.start = env->CallIntMethod(obj,mid_start);\n".format(quote(sym)))
    out.append("\t%s.stride = env->CallIntMethod(obj,mid_stride);\n".format(quote(sym)))
    out.append("\t%s.end = env->CallIntMethod(obj,mid_end);\n".format(quote(sym)))
    out.append("\treturn %s;\n".format(quote(sym)))

    out.toString
  }

  def matrixCopyDtoH(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/MatrixImpl\");\n")
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID manID = env->GetStaticMethodID(cls,\"getDoubleManifest\",\"()Lscala/reflect/ClassManifest;\");\n")
    out.append("\tif(manID==NULL) std::cout << \"getDoubleManifest method NOT found\" << std::endl;\n")
    out.append("\tjobject manifest = env->CallStaticObjectMethod(cls,manID);\n")
    out.append("\tif(manifest==NULL) std::cout << \"manifest object NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IILscala/reflect/ClassManifest;)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.numRows,%s.numCols,manifest);\n".format(quote(sym),quote(sym)))
    out.append("\tif(obj==NULL) std::cout << \"new object NOT created\" << std::endl;\n")

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"doubleData\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tif(mid_data==NULL) std::cout << \"data access method NOT found\" << std::endl;\n")
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tif(dataPtr==NULL) std::cout << \"GetPrimitiveArrayCritical call failed\" << std::endl;\n")

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  def vectorCopyDtoH(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/VectorImpl\");\n")
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID manID = env->GetStaticMethodID(cls,\"getDoubleManifest\",\"()Lscala/reflect/ClassManifest;\");\n")
    out.append("\tif(manID==NULL) std::cout << \"getDoubleManifest method NOT found\" << std::endl;\n")
    out.append("\tjobject manifest = env->CallStaticObjectMethod(cls,manID);\n")
    out.append("\tif(manifest==NULL) std::cout << \"manifest object NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZLscala/reflect/ClassManifest;)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.length,%s.isRow,manifest);\n".format(quote(sym),quote(sym)))
    out.append("\tif(obj==NULL) std::cout << \"new object NOT created\" << std::endl;\n")

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"doubleData\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tif(mid_data==NULL) std::cout << \"data access method NOT found\" << std::endl;\n")
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tif(dataPtr==NULL) std::cout << \"GetPrimitiveArrayCritical call FAILED\" << std::endl;\n")

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  // Generate & register temporary data structures (which could be the output) for GPU kernel
  def emitVectorAlloc(newSym:Sym[_], length:String, isRow:String):Unit = {
    //TODO: Check if both symbols are Vectors

    //Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

    val out = new StringBuilder

    val inputs1 = (gpuOutput :: gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val inputs2 = (gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val paramStrOut = inputs1.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStrOut = inputs1.map("\""+quote(_)+"\"").mkString(",")
    val paramStrTemp = inputs2.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStrTemp = inputs2.map("\""+quote(_)+"\"").mkString(",")

    if(newSym == kernelSymbol)
      out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStrOut))
    else
      out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStrTemp))
    out.append("\t%s %s;\n".format(remap(newSym.Type),quote(newSym)))
    out.append("\t%s *devPtr;\n".format(remap(newSym.Type.typeArguments(0))))
    out.append("\tDeliteCudaMalloc((void**)%s,%s*sizeof(%s));\n".format("&devPtr",length,remap(newSym.Type.typeArguments(0))))
    out.append("\t%s.length = %s;\n".format(quote(newSym),length))
    out.append("\t%s.isRow = %s;\n".format(quote(newSym),isRow))
    out.append("\t%s.data = devPtr;\n".format(quote(newSym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    // Register MetaData
    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s],\"gpuMemCopy_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStrOut,quote(kernelSymbol), quote(newSym), "env", quote(newSym))
      out.append(emitCopyDtoH(newSym))
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStrTemp))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
  }
  def emitVectorAllocSym(newSym:Sym[_], sym:Sym[_]): Unit = {
    emitVectorAlloc(newSym, quote(sym)+".length", quote(sym)+".isRow")
  }

  def emitVectorAllocRef(newSym:Sym[_], sym:Sym[_]): Unit = {

    // Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

    val out = new StringBuilder
    val paramStr = remap(sym.Type) + " " + quote(sym)
    val argStr = "\""+quote(sym)+"\""

    out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStr))
    out.append("\t%s %s = %s;\n".format(remap(newSym.Type),quote(newSym),quote(sym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s],\"gpuMemCopy_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStr,quote(kernelSymbol), quote(newSym), "env", quote(newSym))
      out.append(emitCopyDtoH(newSym))
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStr))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
  }

  def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String): Unit = {
    //TODO: Check if both symbols are Matrices

    //Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

    val out = new StringBuilder

    val inputs1 = (gpuOutput :: gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val inputs2 = (gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val paramStrOut = inputs1.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStrOut = inputs1.map("\""+quote(_)+"\"").mkString(",")
    val paramStrTemp = inputs2.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStrTemp = inputs2.map("\""+quote(_)+"\"").mkString(",")

    if(newSym == kernelSymbol)
      out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStrOut))
    else
      out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStrTemp))
    out.append("\t%s %s;\n".format(remap(newSym.Type),quote(newSym)))
    out.append("\t%s *devPtr;\n".format(remap(newSym.Type.typeArguments(0))))
    out.append("\tDeliteCudaMalloc((void**)%s,%s*%s*sizeof(%s));\n".format("&devPtr",numRows,numCols,remap(newSym.Type.typeArguments(0))))
    out.append("\t%s.numRows = %s;\n".format(quote(newSym),numRows))
    out.append("\t%s.numCols = %s;\n".format(quote(newSym),numCols))
    out.append("\t%s.data = devPtr;\n".format(quote(newSym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    // Register MetaData
    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s],\"gpuMemCopy_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStrOut,quote(kernelSymbol), quote(newSym), "env", quote(newSym))
      out.append(emitCopyDtoH(newSym))
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStrTemp))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
  }
  def emitMatrixAllocSym(newSym:Sym[_], sym:Sym[_]): Unit = {
    emitMatrixAlloc(newSym, quote(sym)+".numRows", quote(sym)+".numCols")
  }

  def emitMatrixAllocRef(newSym:Sym[_], sym:Sym[_]): Unit = {
    // Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

    val out = new StringBuilder
    val paramStr = remap(sym.Type) + " " + quote(sym)
    val argStr = "\""+quote(sym)+"\""

    out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStr))
    out.append("\t%s %s = %s;\n".format(remap(newSym.Type),quote(newSym),quote(sym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s],\"gpuMemCopy_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStr,quote(kernelSymbol), quote(newSym), "env", quote(newSym))
      out.append(emitCopyDtoH(newSym))
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStr))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
  }

}
