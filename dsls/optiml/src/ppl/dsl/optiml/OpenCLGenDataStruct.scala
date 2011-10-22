package ppl.dsl.optiml

import _root_.scala.virtualization.lms.internal.{Expressions, OpenCLCodegen}

/* This trait defines methods for copying datastructures between JVM and GPU */
//TODO: Factor out common things and simplify these methods

trait OpenCLGenDataStruct extends ppl.dsl.optila.OpenCLGenDataStruct {

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
    * Copy Mutable Input from Host to Device
    *************************************************/

  def labelsCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }
  def indexVectorCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }
  def trainingSetCopyMutableInputHtoD(sym: Sym[Any]): String = {
    "assert(false);\n"
  }

}
