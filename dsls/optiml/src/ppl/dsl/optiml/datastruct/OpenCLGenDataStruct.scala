package ppl.dsl.optiml.datastruct

import _root_.scala.virtualization.lms.internal.{Expressions, OpenCLCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait OpenCLGenDataStruct extends OpenCLCodegen {

  val IR: Expressions
  import IR._

  val VectorImplCls = "jclass VectorImplCls = env->FindClass(\"generated/scala/VectorImpl\");\n"
  val VectorViewImplCls = "jclass VectorViewImplCls = env->FindClass(\"generated/scala/VectorViewImpl\");\n"
  val RangeVectorImplCls = "jclass RangeVectorImplCls = env->FindClass(\"generated/scala/RangeVectorImpl\");\n"
  val ZeroVectorImplCls = "jclass ZeroVectorImplCls = env->FindClass(\"generated/scala/ZeroVectorImpl\");\n"
  val EmptyVectorImplCls = "jclass EmptyVectorImplCls = env->FindClass(\"generated/scala/EmptyVectorImpl\");\n"
  val LabelsImplCls = "jclass LabelsImplCls = env->FindClass(\"generated/scala/LabelsImpl\");\n"

  val MatrixImplCls = "jclass MatrixImplCls = env->FindClass(\"generated/scala/MatrixImpl\");\n"
  val MatrixRowImplCls = "jclass MatrixRowImplCls = env->FindClass(\"generated/scala/MatrixRowImpl\");\n"
  val MatrixColImplCls = "jclass MatrixColImplCls = env->FindClass(\"generated/scala/MatrixColImpl\");\n"
  val TrainingSetImplCls = "jclass TrainingSetImplCls = env->FindClass(\"generated/scala/TrainingSetImpl\");\n"

  def vectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->dcSize() * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")
    
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t%s->data = DeliteOpenCLMalloc(%s);\n".format(quote(sym),numBytesStr))

    out.append(RangeVectorImplCls)

    // If RangeVector
    out.append("\tif(env->IsInstanceOf(obj,RangeVectorImplCls)) {\n")
    out.append("\t\tjint start = env->CallIntMethod(obj,env->GetMethodID(cls,\"start\",\"()I\"));\n")
    out.append("\t\tjint end = env->CallIntMethod(obj,env->GetMethodID(cls,\"end\",\"()I\"));\n")
    out.append("\t\tjint stride = env->CallIntMethod(obj,env->GetMethodID(cls,\"stride\",\"()I\"));\n")
    out.append("\t\tint *dataPtr = (int *)malloc(%s);\n".format(numBytesStr))
    out.append("\t\tfor(int i=0;i<%s->length;i++) { dataPtr[i] = start + i*stride; }\n".format(quote(sym)))
    out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data, dataPtr, %s);\n".format(quote(sym),numBytesStr))
    out.append("\t\tfree(dataPtr);\n".format(quote(sym),numBytesStr))
    out.append("\t}\n")

    // If other types of vectors
    out.append("\telse {\n")
    out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data$mc%s$sp\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg),JNITypeDescriptor(typeArg)))
    out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data, dataPtr, %s);\n".format(quote(sym),numBytesStr))
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
    out.append("\t}\n")
    
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def matrixCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->dcSize() * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")

	  // If this is not RangeVector   // TODO: Manage rangevector
    out.append("\t\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t\t%s->numRows = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    out.append("\t\t%s->numCols = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))
    out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t\t%s->data = DeliteOpenCLMalloc(%s);\n".format(quote(sym),numBytesStr))
    out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data, dataPtr, %s);\n".format(quote(sym),numBytesStr))

    // Release
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
    out.append("\t\tenv->DeleteLocalRef(cls);\n")
    out.append("\t\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def rangeVectorCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"isRow\",\"()Z\");\n")
    out.append("\tjmethodID mid_start = env->GetMethodID(cls,\"start\",\"()I\");\n")
    out.append("\tjmethodID mid_stride = env->GetMethodID(cls,\"stride\",\"()I\");\n")
    out.append("\tjmethodID mid_end = env->GetMethodID(cls,\"end\",\"()I\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t%s->start = env->CallIntMethod(obj,mid_start);\n".format(quote(sym)))
    out.append("\t%s->stride = env->CallIntMethod(obj,mid_stride);\n".format(quote(sym)))
    out.append("\t%s->end = env->CallIntMethod(obj,mid_end);\n".format(quote(sym)))
    out.append("\t\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))

    out.toString
  }

  def indexVectorCopyInputHtoD(sym: Sym[Any]): String = {
    //vectorCopyHtoD(sym)
    val out = new StringBuilder
    val typeArg = manifest[Int]
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->dcSize() * sizeof(%s)".format(quote(sym),remap(typeArg))

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
    out.append("\t\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t\t%s->data = DeliteOpenCLMalloc(%s);\n".format(quote(sym),numBytesStr))
    out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data, dataPtr, %s);\n".format(quote(sym),numBytesStr))
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
	  out.append("\t}\n")
	  
    // If this is RangeVector
    out.append("\telse {\n")
    out.append("\t\tjmethodID mid_apply = env->GetMethodID(cls,\"apply\",\"(I)I\");\n")
    out.append("\t\t%s *hostPtr = (%s *)malloc(%s);\n".format(typeStr,typeStr,numBytesStr))
    out.append("\t\t%s->data = DeliteOpenCLMalloc(%s);\n".format(quote(sym),numBytesStr))
	  out.append("\t\tfor(int i=0; i<%s->length; i++) {\n".format(quote(sym)))
    out.append("\t\t\thostPtr[i] = %s;\n".format("env->CallIntMethod(obj,mid_apply,i)"))
	  out.append("\t\t}\n")
    out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data, hostPtr, %s);\n".format(quote(sym),numBytesStr))
	  out.append("\t}\n")


    out.append("\t\tenv->DeleteLocalRef(cls);\n")
    out.append("\t\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def labelsCopyInputHtoD(sym: Sym[Any]): String = {
    vectorCopyInputHtoD(sym)
  }

  def trainingSetCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s->dcSize() * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")

	  // If this is not RangeVector   // TODO: Manage rangevector
    out.append("\t\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t\t%s->numRows = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    out.append("\t\t%s->numCols = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))
    out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t\t%s->data = DeliteOpenCLMalloc(%s);\n".format(quote(sym),numBytesStr))
    out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data, dataPtr, %s);\n".format(quote(sym),numBytesStr))

    // Release
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")

    //TODO: Copy labels
    val typeArg2 = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(1)
    val typeStr2 = remap(typeArg2)
    val numBytesStr2 = "%s->numCols * sizeof(%s)".format(quote(sym),remap(typeArg))

    // Get class, method ID
	  // If this is not RangeVector   // TODO: Manage rangevector
    //out.append("\t\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    //out.append("\t\t%s->length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    //out.append("\t\t%s->isRow = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_isRow)"))
    //out.append("\t\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    //out.append("\t\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    //out.append("\t\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\t\t%s->data_labels = DeliteOpenCLMalloc(sizeof(%s)*%s);\n".format(quote(sym),typeStr2,numBytesStr2))
    //out.append("\t\tDeliteOpenCLMemcpyHtoDAsync(%s->data_labels, dataPtr, %s);\n".format(quote(sym),numBytesStr2))

    // Release
    //out.append("\t\tenv->ReleasePrimitiveArrayCritical(data_labels, data_labelsPtr, 0);\n")
    //out.append("\t\tenv->DeleteLocalRef(data_labels);\n")
    out.append("\t\tenv->DeleteLocalRef(cls);\n")
    out.append("\t\treturn %s;\n".format(quote(sym)))
    out.toString
  }


  /**************************************************
    * Copy Output from Device to Host
    *************************************************/

  def vectorCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.dcSize() * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sVectorImpl\");\n".format(sym.Type.typeArguments(0)))
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.length,%s.isRow);\n".format(quote(sym),quote(sym)))
    out.append("\tif(obj==NULL) std::cout << \"new object NOT created\" << std::endl;\n")

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tDeliteOpenCLMemcpyDtoHAsync(dataPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
    out.append("\t\tenv->DeleteLocalRef(cls);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  def matrixCopyOutputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.dcSize() * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"generated/scala/%sMatrixImpl\");\n".format(sym.Type.typeArguments(0)))
    out.append("\tif(cls==NULL) std::cout << \"class NOT found\" << std::endl;\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
    out.append("\tif(mid==NULL) std::cout << \"constructor NOT found\" << std::endl;\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.numRows,%s.numCols);\n".format(quote(sym),quote(sym)))
    out.append("\tif(obj==NULL) std::cout << \"new object NOT created\" << std::endl;\n")

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tDeliteOpenCLMemcpyDtoHAsync(dataPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
    out.append("\t\tenv->DeleteLocalRef(cls);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  /**************************************************
    * Copy Mutable Input from Host to Device
    *************************************************/
  
  def vectorCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s.dcSize() * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tDeliteOpenCLMemcpyDtoHAsync(dataPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
    out.append("\t\tenv->DeleteLocalRef(cls);\n")

    out.toString
  }

  def matrixCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeArg = if(sym.Type.typeArguments.length==0) manifest[Int] else sym.Type.typeArguments(0)
    val typeStr = remap(typeArg)
    val numBytesStr = "%s.dcSize() * sizeof(%s)".format(quote(sym),remap(typeArg))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tDeliteOpenCLMemcpyDtoHAsync(dataPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(data);\n")
    out.append("\t\tenv->DeleteLocalRef(cls);\n")

    out.toString
  }

  def labelsCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }
  def rangeVectorCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }
  def indexVectorCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }
  def trainingSetCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }


  /*************************************************/

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
      out.append("\t%s->data = DeliteOpenCLMalloc(%s*sizeof(%s));\n".format(quote(newSym),length,remap(newSym.Type.typeArguments(0))))
      if(reset) assert(false)
      out.append("\t%s->length = %s;\n".format(quote(newSym),length))
      out.append("\t%s->isRow = %s;\n".format(quote(newSym),isRow))
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

  def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null):Unit = {
    //TODO: Check if both symbols are Matrices

    //Do not add the same temporary if it already exists
    if(getKernelTemps contains newSym) return

    helperFuncIdx += 1

    val out = new StringBuilder
    val args = (getKernelOutputs ::: getKernelInputs ::: getKernelTemps) filterNot (_==newSym)

    out.append("\t%s *%s = new %s();\n".format(remap(newSym.Type),quote(newSym),remap(newSym.Type)))

    // Check if new allocation is needed
    if(data==null) {
      out.append("\t%s->data = DeliteOpenCLMalloc(%s*%s*sizeof(%s));\n".format(quote(newSym),numRows,numCols,remap(newSym.Type.typeArguments(0))))
      if(reset) assert(false)
      out.append("\t%s->numRows = %s;\n".format(quote(newSym),numRows))
      out.append("\t%s->numCols = %s;\n".format(quote(newSym),numCols))
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

}

