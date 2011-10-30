package ppl.dsl.deliszt.datastruct

import _root_.scala.virtualization.lms.internal.{Expressions, CudaCodegen}
import _root_.scala.virtualization.lms.internal.GenerationFailedException

/* This trait defines methods for copying datastructures between JVM and GPU */

trait CudaGenDataStruct extends CudaCodegen {

  val IR: Expressions
  import IR._

  val CellImplCls = "jclass CellImplCls = env->FindClass(\"generated/scala/CellImpl\");\n"
  val EdgeImplCls = "jclass EdgeImplCls = env->FindClass(\"generated/scala/EdgeImpl\");\n"
  val FaceImplCls = "jclass FaceImplCls = env->FindClass(\"generated/scala/FaceImpl\");\n"
  val VertexImplCls = "jclass VertexImplCls = env->FindClass(\"generated/scala/VertexImpl\");\n"
  val VecImplCls = "jclass VecImplCls = env->FindClass(\"generated/scala/VecImpl\");\n"
  val CRSImplCls = "jclass CRSImplCls = env->FindClass(\"generated/scala/CRSImpl\");\n"

  def writeImplCls: String = {
    val out = new StringBuilder
    out.append(CellImplCls)
    out.append(EdgeImplCls)
    out.append(FaceImplCls)
    out.append(VertexImplCls)
    out.toString
  }

  /* Transfer Mesh Objects (Cell, Edge, Face, Vertex) */
  def MeshObjCopyInputHtoD(sym: Sym[Any]): String = {  ""  }

  /* Transfer function for MeshSet<T> */
  def MeshSetCopyInputHtoD(sym: Sym[Any], argType: Manifest[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(argType)
    val numBytesStr = "%s->size * sizeof(%s)".format(quote(sym),typeStr)

    //out.append(writeImplCls)

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_size = env->GetMethodID(cls,\"size\",\"()I\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->size = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_size)"))
    out.append("\t%s->dir = 1;\n".format(quote(sym)))

    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t%s->data = devPtr;\n".format(quote(sym)))

    /* Iterate over the input array to retrieve the values from the object type elements */
    //TODO: Since we know the type of the return meshset, we can calculate the result value here instead of calling apply method for each element
    out.append("\tfor(int i=0; i<%s->size; i++) { \n".format(quote(sym)))
    out.append("\t\tjmethodID mid_elem = env->GetMethodID(cls,\"apply\",\"(I)I\");\n")
    /*
    remap(argType) match {
      case "Cell" => out.append("\t\tjmethodID mid_id = env->GetMethodID(CellImplCls,\"id\",\"()I\");\n")
      case "Edge" => out.append("\t\tjmethodID mid_id = env->GetMethodID(EdgeImplCls,\"id\",\"()I\");\n")
      case "Face" => out.append("\t\tjmethodID mid_id = env->GetMethodID(FaceImplCls,\"id\",\"()I\");\n")
      case "Vertex" => out.append("\t\tjmethodID mid_id = env->GetMethodID(VertexImplCls,\"id\",\"()I\");\n")
      case "int" => out.append("\t\tjmethodID mid_id = env->GetMethodID(IntImplCls,\"id\",\"()I\");\n")
      case _ => throw new GenerationFailedException("Cuda: Cannot find method ID for MeshSet Transfer")
    }
    */
    out.append("\t\thostPtr[i] = env->CallIntMethod(obj,mid_elem,i);\n")
    out.append("\t}\n")
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def MeshSetCopyOutputDtoH(sym: Sym[Any], argType: Manifest[_]): String = {
    "//TODO: Implement this!\n"
  }

  def MeshSetCopyMutableInputDtoH(sym: Sym[Any], argType: Manifest[_]): String = {
    "//TODO: Implement this!\n"
  }


  /* Transfer function for Field<T> (T:Primitve Types) */
  def FieldCopyInputHtoD(sym: Sym[Any], argType: Manifest[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(argType)
    val numBytesStr = "%s->size * sizeof(%s)".format(quote(sym),typeStr)

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_size = env->GetMethodID(cls,\"size\",\"()I\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->size = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_size)"))

    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t%s->data = devPtr;\n".format(quote(sym)))

    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data$mc%s$sp\",\"()[%s\");\n".format(JNITypeDescriptor(argType),JNITypeDescriptor(argType)))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s->data, hostPtr, %s);\n".format(quote(sym),numBytesStr))
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    out.append("\tenv->DeleteLocalRef(data);\n")
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def FieldCopyOutputDtoH(sym: Sym[Any], argType: Manifest[_]): String = {
    "//TODO: Implement this!\n"
  }

  def FieldCopyMutableInputDtoH(sym: Sym[Any], argType: Manifest[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(argType)
    val numBytesStr = "%s.size * sizeof(%s)".format(quote(sym),remap(argType))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data$mc%s$sp\",\"()[%s\");\n".format(JNITypeDescriptor(argType),JNITypeDescriptor(argType)))
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

  /* Transfer function for Field<T> (T:Short Vector Types) */
  def VecFieldCopyInputHtoD(sym: Sym[Any], argType: Manifest[_], size: Int): String = {
    val out = new StringBuilder
    val typeStr = remap(argType)
    val numBytesStr = "%s->size * sizeof(%s) * %s".format(quote(sym),typeStr,size)

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_size = env->GetMethodID(cls,\"size\",\"()I\");\n")

    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
    out.append("\t%s->size = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_size)"))

    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))
    out.append("\t%s->data = devPtr;\n".format(quote(sym)))

    out.append("\t"+VecImplCls)
    out.append("\tjmethodID mid_elem = env->GetMethodID(cls,\"apply\",\"(I)Ljava/lang/Object;\");\n")
    out.append("\tjmethodID mid_elem_data = env->GetMethodID(VecImplCls,\"data$mc%s$sp\",\"()[%s\");\n".format(JNITypeDescriptor(argType),JNITypeDescriptor(argType)))

    out.append("\tfor(int i=0; i<%s->size; i++) {\n".format(quote(sym)))
    out.append("\t\tjobject elem = env->CallObjectMethod(obj,mid_elem,i);\n")
    out.append("\t\tj%sArray elem_data = (j%sArray)(env->CallObjectMethod(elem,mid_elem_data));\n".format(typeStr,typeStr))
    out.append("\t\tj%s *elem_data_ptr = (j%s *)env->GetPrimitiveArrayCritical(elem_data,0);\n".format(typeStr,typeStr))
    out.append("\t\tfor(int j=0; j<%s; j++) { hostPtr[i*%s+j] = elem_data_ptr[j]; }\n".format(size,size))
    out.append("\t\tenv->ReleasePrimitiveArrayCritical(elem_data, elem_data_ptr, 0);\n")
    out.append("\t\tenv->DeleteLocalRef(elem_data);\n")
    out.append("\t}\n")
    out.append("\tDeliteCudaMemcpyHtoDAsync(devPtr, hostPtr, %s);\n".format(numBytesStr))

    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def VecFieldCopyOutputDtoH(sym: Sym[Any], argType: Manifest[_], size: Int): String = {
    "//TODO: Implement this!\n"
  }

  def VecFieldCopyMutableInputDtoH(sym: Sym[Any], argType: Manifest[_], size: Int): String = {
    "//TODO: Implement this!\n"
  }

  /* Transfer Mesh */
  def MeshCopyInputHtoD(sym: Sym[Any]): String = {
    val out = new StringBuilder

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))

    //Copy Int fields
    for (name <- List("nvertices","nedges","nfaces","ncells")) {
      out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()I\");\n".format(name,name))
      out.append("\t%s->%s = env->CallIntMethod(obj,mid_%s);\n".format(quote(sym),name,name))
    }

    //Copy CRS fields
    out.append("\tint *hostPtr;\n")
    out.append("\tint *devPtr;\n")
    out.append("\t"+CRSImplCls)
    for (crs <- List("vtov","vtoe","vtof","vtoc","etov","etof","etoc","ftov","ftoe","ftoc","ctov","ctoe","ctof","ctoc")) {
      out.append("\tCRS %s;\n".format(crs))
      out.append("\tjmethodID mid_%s = env->GetMethodID(cls,\"%s\",\"()Lgenerated/scala/CRS;\");\n".format(crs,crs))
      out.append("\tjobject obj_%s = env->CallObjectMethod(obj,mid_%s);\n".format(crs,crs))
      out.append("\tjclass cls_%s = env->GetObjectClass(obj_%s);\n".format(crs,crs))
      for (array <- List("values","rows")) {
        if (array == "rows") {
          out.append("\tif(env->IsInstanceOf(obj_%s,CRSImplCls)) {\n".format(crs))
        }
        out.append("\tjmethodID mid_%s_%s = env->GetMethodID(cls_%s,\"%s\",\"()[I\");\n".format(crs,array,crs,array))
        out.append("\tjintArray %s_%s = (jintArray)(env->CallObjectMethod(obj_%s,mid_%s_%s));\n".format(crs,array,crs,crs,array))
        out.append("\tint %s_%s_size = 4 * env->GetArrayLength(%s_%s);\n".format(crs,array,crs,array))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s_%s_size);\n".format(crs,array))
        out.append("\tDeliteCudaMalloc((void**)&devPtr,%s_%s_size);\n".format(crs,array))
        out.append("\tjint *%s_%s_ptr = (jint *)env->GetPrimitiveArrayCritical(%s_%s,0);\n".format(crs,array,crs,array))
        out.append("\tmemcpy(hostPtr, %s_%s_ptr, %s_%s_size);\n".format(crs,array,crs,array))
        out.append("\tDeliteCudaMemcpyHtoDAsync(devPtr, hostPtr, %s_%s_size);\n".format(crs,array))
        out.append("\t%s.%s = devPtr;\n".format(crs,array))
        out.append("\tenv->ReleasePrimitiveArrayCritical(%s_%s, %s_%s_ptr, 0);\n".format(crs,array,crs,array))
        out.append("\tenv->DeleteLocalRef(%s_%s);\n".format(crs,array))
        if (array == "rows") {
          out.append("\t}\n")
          out.append("\telse {\n")    // When CRSConst
          out.append("\tjmethodID mid_%s_mult = env->GetMethodID(cls_%s,\"mult\",\"()I\");\n".format(crs,crs))
          out.append("\tjint %s_mult = (jint)(env->CallIntMethod(obj_%s,mid_%s_mult));\n".format(crs,crs,crs))
          out.append("\tjint %s_num_rows = 1 + %s_values_size / 4 / %s_mult;\n".format(crs,crs,crs))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s_num_rows*4);\n".format(crs))
          out.append("\tDeliteCudaMalloc((void**)&devPtr,%s_num_rows*4);\n".format(crs))
          out.append("\tfor(int i=0; i<%s_num_rows; i++) { hostPtr[i] = %s_mult * i; }\n".format(crs,crs))
          out.append("\tDeliteCudaMemcpyHtoDAsync(devPtr, hostPtr, %s_num_rows*4);\n".format(crs))
          out.append("\t%s.%s = devPtr;\n".format(crs,array))
          out.append("\t}\n")
        }
      }
      out.append("\t%s->%s = %s;\n".format(quote(sym),crs,crs))
    }

    //TODO: Copy MeshSet fields
    for (tpe <- List("Cell","Edge","Face","Vertex")) {
      out.append("\tMeshSet<%s> meshset_%s;\n".format(tpe,tpe))
    }

    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def MeshCopyOutputDtoH(sym: Sym[Any]): String = {
    "//TODO: Implement this!\n"
  }

  def MeshCopyMutableInputDtoH(sym: Sym[Any]): String = {
    "//TODO: Implement this!\n"
  }

  def VecCopyInputHtoD(sym: Sym[Any], argType: Manifest[_], size: Int): String = {
    val out = new StringBuilder
    val typeStr = remap(argType)

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))

    out.append("\t" + VecImplCls)
    out.append("\tjmethodID mid_elem = env->GetMethodID(VecImplCls,\"apply$mc%s$sp\",\"(I)%s\");\n".format(JNITypeDescriptor(argType),JNITypeDescriptor(argType)))
    out.append("\tfor(int i=0; i<%s; i++) {\n".format(size))
    out.append("\t\t%s->data[i] = env->CallIntMethod(obj,mid_elem,i);\n")
    out.append("\t}\n")

    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def VecCopyOutputDtoH(sym: Sym[Any], argType: Manifest[_]): String = {
    "//TODO: Implement this!\n"
  }
  def VecCopyMutableInputDtoH(sym: Sym[Any], argType: Manifest[_]): String = {
    "//TODO: Implement this!\n"
  }



  def matCopyInputHtoD(sym: Sym[Any]): String = { "" }
  def matCopyOutputDtoH(sym: Sym[Any]): String = { "" }
  def matCopyMutableInputDtoH(sym: Sym[Any]): String = { "" }


  // Dummy methods temporarily just for the compilation
  def emitVecAlloc(newSym:Sym[_],length:String,reset:Boolean,data:String=null) {}
  def emitVecAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  def emitVecAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}
  def emitMatAlloc(newSym:Sym[_], numRows:String, numCols:String, reset:Boolean, data:String=null) {}
  def emitMatAllocSym(newSym:Sym[_], sym:Sym[_], reset:Boolean=false) {}
  def emitMatAllocRef(newSym:Sym[Any], sym:Sym[Any]) {}

}

