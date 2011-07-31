package ppl.dsl.optiml.datastruct

import _root_.scala.virtualization.lms.internal.{Expressions, CudaCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait CudaGenDataStruct extends CudaCodegen {

  val IR: Expressions
  import IR._

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

  def vectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")

	out.append("\tjclass viewCls = env->FindClass(\"generated/scala/%sVectorViewImpl\");\n".format(typeArg.toString));
	out.append("\tjboolean isViewCls = env->IsInstanceOf(obj,viewCls);\n");
	out.append("\tjclass zeroCls = env->FindClass(\"generated/scala/VectorImpl\");\n");
	out.append("\tjboolean isZeroCls = env->IsInstanceOf(obj,zeroCls);\n");

	  // If this is not RangeVector
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    
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

  def indexVectorCopyInputHtoD(sym: Sym[Any]): String = {
    //vectorCopyHtoD(sym)
    val out = new StringBuilder
    val typeArg = manifest[Int]
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")

	out.append("\tjclass rangeCls = env->FindClass(\"generated/scala/IndexVectorRangeImpl\");\n");
	out.append("\tjboolean isRangeCls = env->IsInstanceOf(obj,rangeCls);\n");

    out.append("\t\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
	
	// If this is not RangeVector
	out.append("\tif(isRangeCls == false) {\n")
    out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
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
    out.append("\t\t%s->data = %s;\n".format(quote(sym),"devPtr"))
    // Release
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
	out.append("\t}\n")

	out.append("\telse {\n")
    out.append("\t\t%s *hostPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t\tjfieldID fid_start = env->GetFieldID(cls,\"start\",\"I\");\n")
    out.append("\t\tjint start = env->GetIntField(obj,fid_start);\n")
	out.append("\t\tfor(int i=0; i<%s->length; i++) {\n".format(quote(sym)))
    out.append("\t\t\thostPtr[i] = start + i;\n")
	out.append("\t\t}\n")
    out.append("\t\t%s *devPtr;\n".format(typeStr))
    out.append("\t\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    out.append("\t\t%s->data = %s;\n".format(quote(sym),"devPtr"))
	out.append("\t}\n")


    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\t\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def labelsCopyInputHtoD(sym: Sym[Any]): String = {
    vectorCopyInputHtoD(sym)
  }

  def trainingSetCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Copy TrainingSet numRows, numCols, data
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\t%s->numRows = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    out.append("\t%s->numCols = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    out.append("\t%s->data = %s;\n".format(quote(sym),"devPtr"))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")

    // Get object fields (labels / transposed)
    out.append("\tjmethodID fid_labels = env->GetMethodID(cls,\"labels\",\"()Lgenerated/scala/Labels;\");\n")
    out.append("\tjfieldID fid_transposed = env->GetFieldID(cls,\"transposed\",\"Lgenerated/scala/%s%sTrainingSetImpl;\");\n".format(sym.Type.typeArguments(0).toString,sym.Type.typeArguments(0).toString))
    out.append("\tjobject obj_labels = env->CallObjectMethod(obj,fid_labels);\n")
    out.append("\tjobject obj_transposed = env->GetObjectField(obj,fid_transposed);\n")

    // Copy Labels 
    val typeStr_labels = remap(sym.Type.typeArguments(1))
    val numBytesStr_labels = "labels.length * sizeof(%s)".format(typeStr_labels)
    out.append("\tLabels<%s> labels;\n".format(typeStr_labels))
    out.append("\tlabels.length = %s->numRows;\n".format(quote(sym)))
    out.append("\tlabels.isRow = false;\n")
    out.append("\tjclass cls_labels = env->GetObjectClass(obj_labels);\n")
    out.append("\tjmethodID mid_data_labels = env->GetMethodID(cls_labels,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(1))))
    out.append("\tj%sArray data_labels = (j%sArray)(%s);\n".format(typeStr_labels,typeStr_labels,"env->CallObjectMethod(obj_labels,mid_data_labels)"))
    out.append("\tj%s *dataPtr_labels = (j%s *)env->GetPrimitiveArrayCritical(data_labels,0);\n".format(typeStr_labels,typeStr_labels))
    out.append("\t%s *hostPtr_labels;\n".format(typeStr_labels))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr_labels",numBytesStr_labels))
    out.append("\t%s *devPtr_labels;\n".format(typeStr_labels))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr_labels",numBytesStr_labels))
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr_labels","dataPtr_labels",numBytesStr_labels))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr_labels","hostPtr_labels",numBytesStr_labels))
    out.append("\tlabels.data = devPtr_labels;\n")
    out.append("\tenv->ReleasePrimitiveArrayCritical(data_labels, dataPtr_labels, 0);\n")
    out.append("\t%s->labels = labels;\n".format(quote(sym)))
    //out.append("\tDeliteCudaMalloc((void**)%s,sizeof(Labels<%s>));\n".format("&labels",typeStr_labels))

    // Copy Transposed 
    out.append("\t%s transposed;\n".format(remap(sym.Type)))
    out.append("\ttransposed.numRows = %s->numCols;\n".format(quote(sym)))
    out.append("\ttransposed.numCols = %s->numRows;\n".format(quote(sym)))
    out.append("\ttransposed.labels = labels;\n")
    
	out.append("\tjclass cls_transposed = env->GetObjectClass(obj_transposed);\n")
    out.append("\tjmethodID mid_data_transposed = env->GetMethodID(cls_transposed,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data_transposed = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj_transposed,mid_data_transposed)"))
    out.append("\tj%s *dataPtr_transposed = (j%s *)env->GetPrimitiveArrayCritical(data_transposed,0);\n".format(typeStr,typeStr))
    
    out.append("\t%s *devPtr_transposed;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s+sizeof(%s));\n".format("&devPtr_transposed",numBytesStr,remap(sym.Type)))
	out.append("\ttransposed.data = devPtr_transposed;\n")
    out.append("\t%s *hostPtr_transposed;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s+sizeof(%s));\n".format("&hostPtr_transposed",numBytesStr,remap(sym.Type)))
    out.append("\t%s *hostPtr_transposed_cls = hostPtr_transposed + %s->size();\n".format(typeStr,quote(sym)))
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr_transposed","dataPtr_transposed",numBytesStr))
    out.append("\tmemcpy(%s, %s, sizeof(%s));\n".format("hostPtr_transposed_cls","&transposed",remap(sym.Type)))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s+sizeof(%s));\n".format("devPtr_transposed","hostPtr_transposed",numBytesStr,remap(sym.Type)))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data_transposed, dataPtr_transposed, 0);\n")
    out.append("\tenv->DeleteLocalRef(data_transposed);\n")
    out.append("\t%s->transposed = (%s *)(devPtr_transposed + %s->size());\n".format(quote(sym),remap(sym.Type),quote(sym)))

    out.append("\tenv->DeleteLocalRef(cls_labels);\n")
    out.append("\tenv->DeleteLocalRef(cls_transposed);\n")
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

  def indexVectorCopyMutableInputDtoH(sym: Sym[Any]): String = {
    vectorCopyMutableInputDtoH(sym)
  }

  def labelsCopyMutableInputDtoH(sym: Sym[Any]): String = {
    vectorCopyMutableInputDtoH(sym)
  }

  def trainingSetCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    out.append("\tstd::cout << \"CopyDtoHBack TrainingSet is not supported yet\" << std::endl;\n")
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
    if(gpuTemps.contains(newSym)) return

	helperFuncIdx += 1

    val out = new StringBuilder

    val inputs1 = (gpuOutputs ::: gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val inputs2 = (gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val paramStrOut = inputs1.map(ele=>
		if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
		else remap(ele.Type) + " " + quote(ele)
	).mkString(",")
    val argStrOut = inputs1.map("\""+quote(_)+"\"").mkString(",")
    val paramStrTemp = inputs2.map(ele=>
		if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
		else remap(ele.Type) + " " + quote(ele)
	).mkString(",")
    val argStrTemp = inputs2.map("\""+quote(_)+"\"").mkString(",")

	/*
    if(newSym == kernelSymbol)
      out.append("%s *gpuMemAlloc_%s_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx, paramStrOut))
    else
      out.append("%s *gpuMemAlloc_%s_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,paramStrTemp))
    */
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
    //out.append("}\n")

	val allocStr = emitAllocOutput(newSym, null, out.toString, inputs1)
    helperFuncString.append(allocStr)
	
	val copyStr = emitCopyOutputDtoH(newSym, null, copyOutputDtoH(newSym)) 
    helperFuncString.append(copyStr)
	    
	gpuOutputs = gpuOutputs :+ newSym
    
	/*
	// Register MetaData
    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s_%s\",[%s],\"gpuMemCopy_%s_%s_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,argStrOut,quote(kernelSymbol), quote(newSym), helperFuncIdx,"env", quote(newSym))
      out.append(emitCopyOutputDtoH(newSym))
	    gpuOutputs = gpuOutputs :+ newSym
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),helperFuncIdx,argStrTemp))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
	*/
  }

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
    if(gpuTemps.contains(newSym)) return

	  helperFuncIdx += 1

    val out = new StringBuilder

    val inputs1 = (gpuOutputs ::: gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val inputs2 = (gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val paramStrOut = inputs1.map(ele=>
			if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
			else remap(ele.Type) + " " + quote(ele)
	).mkString(",")
    val argStrOut = inputs1.map("\""+quote(_)+"\"").mkString(",")
    val paramStrTemp = inputs2.map(ele=>
			if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
			else remap(ele.Type) + " " + quote(ele)
	).mkString(",")
    val argStrTemp = inputs2.map("\""+quote(_)+"\"").mkString(",")

	
	/*
    if(newSym == kernelSymbol)
      out.append("%s *allocFunc_%s(%s) {\n".format(remap(newSym.Type),helperFuncIdx,paramStrOut))
    else
      out.append("%s *allocFunc_%s(%s) {\n".format(remap(newSym.Type),helperFuncIdx,paramStrTemp))
    */
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
      out.append("\t%s->data = %sr;\n".format(quote(newSym),data))
    }
    out.append("\treturn %s;\n".format(quote(newSym)))
    //out.append("}\n")

	val allocStr = emitAllocOutput(newSym, null, out.toString, inputs1)
    helperFuncString.append(allocStr)
	
	val copyStr = emitCopyOutputDtoH(newSym, null, copyOutputDtoH(newSym)) 
    helperFuncString.append(copyStr)

	//TODO: Need to do this here? Or in the Cudagen?
	gpuOutputs = gpuOutputs :+ newSym
		/*
    // Register MetaData
    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"%s\",\"allocFunc_%s\",[%s],\"copyOutputDtoH_%s\",[\"%s\",\"%s\"]]}".format(quote(newSym),remap(newSym.Type),helperFuncIdx,argStrOut,helperFuncIdx,"env", quote(newSym))
      out.append(copyOutputDtoH(newSym))
	    gpuOutputs = gpuOutputs :+ newSym
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":[\"%s\",\"allocFunc_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),helperFuncIdx,argStrTemp))
      gpuTemps = gpuTemps :+ newSym
    }
    helperFuncString.append(out.toString)
	*/

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
}

