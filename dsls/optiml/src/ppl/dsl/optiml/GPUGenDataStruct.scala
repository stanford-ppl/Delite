package ppl.dsl.optiml

import _root_.scala.virtualization.lms.internal.{Expressions, CudaCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
trait CudaGenDataStruct extends ppl.dsl.optila.CudaGenDataStruct with GPUGenDataStruct
trait OpenCLGenDataStruct extends ppl.dsl.optila.OpenCLGenDataStruct with GPUGenDataStruct

trait GPUGenDataStruct extends ppl.dsl.optila.GPUGenDataStruct {

  val IR: Expressions
  import IR._

  val ZeroVectorImplCls = "jclass ZeroVectorImplCls = env->FindClass(\"generated/scala/ZeroVectorImpl\");\n"
  val EmptyVectorImplCls = "jclass EmptyVectorImplCls = env->FindClass(\"generated/scala/EmptyVectorImpl\");\n"
  val LabelsImplCls = "jclass LabelsImplCls = env->FindClass(\"generated/scala/LabelsImpl\");\n"

  val MatrixRowImplCls = "jclass MatrixRowImplCls = env->FindClass(\"generated/scala/MatrixRowImpl\");\n"
  val MatrixColImplCls = "jclass MatrixColImplCls = env->FindClass(\"generated/scala/MatrixColImpl\");\n"
  val TrainingSetImplCls = "jclass TrainingSetImplCls = env->FindClass(\"generated/scala/TrainingSetImpl\");\n"

  
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

    out.append("\t\t%s *%s = new %s();\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
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
    densevectorCopyInputHtoD(sym)
  }

  def trainingSetCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.tp.typeArguments(0))
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.tp.typeArguments(0)))

    // Copy TrainingSet numRows, numCols, data
    out.append("\t%s *%s = new %s();\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.tp.typeArguments(0))))
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
    out.append("\tjfieldID fid_transposed = env->GetFieldID(cls,\"transposed\",\"Lgenerated/scala/%s%sTrainingSetImpl;\");\n".format(sym.tp.typeArguments(0).toString,sym.tp.typeArguments(1).toString))
    out.append("\tjobject obj_labels = env->CallObjectMethod(obj,fid_labels);\n")
    out.append("\tjobject obj_transposed = env->GetObjectField(obj,fid_transposed);\n")

    // Copy Labels 
    val typeStr_labels = remap(sym.tp.typeArguments(1))
    val numBytesStr_labels = "labels.length * sizeof(%s)".format(typeStr_labels)
    out.append("\tLabels<%s> labels;\n".format(typeStr_labels))
    out.append("\tlabels.length = %s->numRows;\n".format(quote(sym)))
    out.append("\tlabels.isRow = false;\n")
    out.append("\tjclass cls_labels = env->GetObjectClass(obj_labels);\n")
    out.append("\tjmethodID mid_data_labels = env->GetMethodID(cls_labels,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.tp.typeArguments(1))))
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
    out.append("\t%s transposed;\n".format(remap(sym.tp)))
    out.append("\ttransposed.numRows = %s->numCols;\n".format(quote(sym)))
    out.append("\ttransposed.numCols = %s->numRows;\n".format(quote(sym)))
    out.append("\ttransposed.labels = labels;\n")
    
	out.append("\tjclass cls_transposed = env->GetObjectClass(obj_transposed);\n")
    out.append("\tjmethodID mid_data_transposed = env->GetMethodID(cls_transposed,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.tp.typeArguments(0))))
    out.append("\tj%sArray data_transposed = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj_transposed,mid_data_transposed)"))
    out.append("\tj%s *dataPtr_transposed = (j%s *)env->GetPrimitiveArrayCritical(data_transposed,0);\n".format(typeStr,typeStr))
    
    out.append("\t%s *devPtr_transposed;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s+sizeof(%s));\n".format("&devPtr_transposed",numBytesStr,remap(sym.tp)))
	out.append("\ttransposed.data = devPtr_transposed;\n")
    out.append("\t%s *hostPtr_transposed;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s+sizeof(%s));\n".format("&hostPtr_transposed",numBytesStr,remap(sym.tp)))
    out.append("\t%s *hostPtr_transposed_cls = hostPtr_transposed + %s->size();\n".format(typeStr,quote(sym)))
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr_transposed","dataPtr_transposed",numBytesStr))
    out.append("\tmemcpy(%s, %s, sizeof(%s));\n".format("hostPtr_transposed_cls","&transposed",remap(sym.tp)))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s+sizeof(%s));\n".format("devPtr_transposed","hostPtr_transposed",numBytesStr,remap(sym.tp)))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data_transposed, dataPtr_transposed, 0);\n")
    out.append("\tenv->DeleteLocalRef(data_transposed);\n")
    out.append("\t%s->transposed = (%s *)(devPtr_transposed + %s->size());\n".format(quote(sym),remap(sym.tp),quote(sym)))

    out.append("\tenv->DeleteLocalRef(cls_labels);\n")
    out.append("\tenv->DeleteLocalRef(cls_transposed);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def indexVectorCopyMutableInputDtoH(sym: Sym[Any]): String = {
    densevectorCopyMutableInputDtoH(sym)
  }

  def labelsCopyMutableInputDtoH(sym: Sym[Any]): String = {
    densevectorCopyMutableInputDtoH(sym)
  }

  def trainingSetCopyMutableInputDtoH(sym: Sym[Any]): String = {
    val out = new StringBuilder
    out.append("\tstd::cout << \"CopyDtoHBack TrainingSet is not supported yet\" << std::endl;\n")
    out.toString
  }
}
