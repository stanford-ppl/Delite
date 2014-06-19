package ppl.delite.framework.datastructures

import scala.virtualization.lms.internal.{GenerationFailedException, Expressions, CudaDeviceTransfer, CudaCodegen}
import scala.virtualization.lms.common.BaseGenStruct
import scala.virtualization.lms.internal.Targets._

trait DeliteCudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen with CLikeGenDeliteStruct =>

  val IR: Expressions
  import IR._

  override def emitSendSlave(tp: Manifest[_]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "%sRef%s *sendCuda_%s(%sRef%s *sym)".format(deviceTarget,remap(typeArg),mangledName(deviceTarget+"Ref"+remap(tp)),hostTarget,remap(typeArg))
      out.append(signature + " {\n")
      out.append("\t%sRef%s *sym_dev = new %sRef%s(sym->get());\n".format(deviceTarget,remap(typeArg),deviceTarget,remap(typeArg)))
      out.append("\treturn sym_dev;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "%s *sendCuda_%s(%s *sym)".format(remap(tp),mangledName(remap(tp)),remapHost(tp))
      out.append(signature + " {\n")
      out.append("\t%s *sym_dev = new %s();\n".format(remap(tp),remap(tp)))
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(isPrimitiveType(elemtp)) {
          out.append("\tsym_dev->%s = sym->%s;\n".format(elem._1,elem._1))
        }
        else {
          out.append("\tsym_dev->%s = *sendCuda_%s(sym->%s);\n".format(elem._1,mangledName(remap(elemtp)),elem._1))
        }
      }
      out.append("\treturn sym_dev;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (isArrayType(tp)) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if(isPrimitiveType(typeArg)) {
        val signature = "%s *sendCuda_%s(%s *sym)".format(remap(tp),mangledName(remap(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\tmemcpy(hostPtr, sym->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\t%s *sym_dev = new %s(sym->length);\n".format(remap(tp),remap(tp)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(sym_dev->data, hostPtr, sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\treturn sym_dev;\n")
        out.append("}\n")
        val signatureT = "%s *sendCudaTrans_%s(%s *sym, int stride)".format(remap(tp),mangledName(remap(tp)),remapHost(tp))
        out.append(signatureT + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\tint numCols = stride;\n")
        out.append("\tint numRows = sym->length / stride;\n")
        out.append("\tfor(int i=0; i<numRows; i++) {\n")
        out.append("\t\tfor(int j=0; j<numCols; j++) {\n")
        out.append("\t\t\thostPtr[j*numRows+i] = sym->data[i*numCols+j];\n")
        out.append("\t\t}\n")
        out.append("\t}\n")
        out.append("\t%s *sym_dev = new %s(sym->length);\n".format(remap(tp),remap(tp)))
        out.append("\tsym_dev->offset = 0; sym_dev->stride = numRows; sym_dev->flag = numCols;\n")
        out.append("\tDeliteCudaMemcpyHtoDAsync(sym_dev->data, hostPtr, sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\treturn sym_dev;\n")
        out.append("}\n")
        (signature+";\n" + signatureT+";\n", out.toString)
      }
      else {
        val signature = "%s *sendCuda_%s(%s *sym)".format(remap(tp),mangledName(remap(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\tfor(int i=0; i<sym->length; i++) {\n")
        out.append("\t\thostPtr[i] = *sendCuda_%s(sym->data[i]);\n".format(mangledName(remap(typeArg))))
        out.append("\t}\n")
        out.append("\t%s *sym_dev = new %s(sym->length);\n".format(remap(tp),remap(tp)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(sym_dev->data, hostPtr, sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\treturn sym_dev;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
    }
    else {
      super.emitSendSlave(tp)
    }
  }

  override def emitRecvSlave(tp: Manifest[_]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "%sRef%s *recvCuda_%s(%sRef%s *sym_dev)".format(hostTarget,remapHost(typeArg),mangledName(deviceTarget+"Ref"+remap(tp)),deviceTarget,remap(typeArg))
      out.append(signature + " {\n")
      out.append("assert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "%s *recvCuda_%s(%s *sym_dev)".format(remapHost(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\t%s *sym = new %s();\n".format(remapHost(tp),remapHost(tp)))
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(isPrimitiveType(elemtp)) {
          out.append("\tsym->%s = sym_dev->%s;\n".format(elem._1,elem._1))
        }
        else {
          out.append("\tsym->%s = recvCuda_%s(&(sym_dev->%s));\n".format(elem._1,mangledName(remap(elemtp)),elem._1))
        }
      }
      out.append("\treturn sym;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (isArrayType(tp)) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      val signature = "%s *recvCuda_%s(%s *sym_dev)".format(remapHost(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      if(isPrimitiveType(typeArg)) {
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym_dev->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, sym_dev->data, sym_dev->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\t%s *sym = new %s(sym_dev->length);\n".format(remapHost(tp),remapHost(tp)))
        out.append("\tmemcpy(sym->data,hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\treturn sym;\n")
      }  
      else {
        out.append("\t%s *sym = new %s(sym_dev->length);\n".format(remapHost(tp),remapHost(tp)))
        out.append("\t%s *temp = (%s*)malloc(sizeof(%s)*sym_dev->length);\n".format(remap(typeArg),remap(typeArg),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync((void*)temp, (void*)(sym_dev->data), sym_dev->length*sizeof(%s));\n".format(remap(typeArg)))
        out.append("\tsym->length = sym_dev->length;\n")
        out.append("\tfor(int i=0; i<sym->length; i++) {\n")
        out.append("\t\tsym->data[i] = recvCuda_%s(temp+i);\n".format(mangledName(remap(typeArg))))
        out.append("\t}\n")
        out.append("\treturn sym;\n")
      }
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else
      super.emitRecvSlave(tp)
  }

  /*
  override def emitSendViewSlave(sym: Sym[Any]): (String,String) = {
  }
  override def emitRecvViewSlave(sym: Sym[Any]): (String,String) = {
  }
  */

  override def emitSendUpdateSlave(tp: Manifest[_]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "void sendUpdateCuda_%s(%sRef%s *sym_dev, %sRef%s *sym)".format(mangledName(deviceTarget+"Ref"+remap(tp)),deviceTarget,remap(typeArg),hostTarget,remap(typeArg))
      out.append(signature + " {\n")
      out.append("sym_dev->data = sym->data;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "void sendUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remapHost(tp))
      out.append(signature + " {\n")
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(isPrimitiveType(elemtp)) {
          out.append("\tsym_dev->%s = sym->%s;\n".format(elem._1,elem._1))
        }
        else {
          out.append("\tsendUpdateCuda_%s(&(sym_dev->%s),sym->%s);\n".format(mangledName(remap(elemtp)),elem._1,elem._1))
        }
      }
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (isArrayType(tp)) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      val signature = "void sendUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remapHost(tp))
      out.append(signature + " {\n")
      out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
      out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
      out.append("\tmemcpy(hostPtr, sym->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
      out.append("\tDeliteCudaMemcpyHtoDAsync(sym_dev->data, hostPtr, sym->length*sizeof(%s));\n".format(remap(typeArg)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      super.emitSendUpdateSlave(tp)
    }
  }

  override def emitRecvUpdateSlave(tp: Manifest[_]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "void recvUpdateCuda_%s(%sRef%s *sym_dev, %sRef%s *sym)".format(mangledName(deviceTarget+"Ref"+remap(tp)),deviceTarget,remap(typeArg),hostTarget,remap(typeArg))
      out.append(signature + " {\n")
      out.append("assert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "void recvUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remapHost(tp))
      out.append(signature + " {\n")
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(isPrimitiveType(elemtp)) {
          out.append("\tsym->%s = sym_dev->%s;\n".format(elem._1,elem._1))
        }
        else { // Always assume array type?
          out.append("\trecvUpdateCuda_%s(&(sym_dev->%s), sym->%s);\n".format(mangledName(remap(elemtp)),elem._1,elem._1))
        }
      }
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (isArrayType(tp)) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      val signature = "void recvUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remapHost(tp))
      out.append(signature + " {\n")
      out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
      out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
      out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, sym_dev->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
      out.append("\tmemcpy(sym->data,hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      super.emitRecvUpdateSlave(tp)
    }
  }

}
