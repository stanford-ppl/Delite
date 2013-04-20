package ppl.delite.framework.datastructures

import scala.virtualization.lms.internal.{GenerationFailedException, Expressions, CudaDeviceTransfer, CudaCodegen}
import scala.virtualization.lms.common.BaseGenStruct


trait DeliteCudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen with BaseGenStruct =>

  val IR: Expressions
  import IR._

  private def isVarType[T](m: Manifest[T]) = m.erasure.getSimpleName == "Variable"
  private def baseType[T](m: Manifest[T]) = if (isVarType(m)) mtype(m.typeArguments(0)) else m

  override def emitSendSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "Ref< %s > *sendCuda_Ref__%s__(HostRef< %s > *sym)".format(remap(typeArg),mangledName(remap(tp)),remap(typeArg))
      out.append(signature + " {\n")
      out.append("\tRef< %s > *sym_dev = new Ref< %s >(sym->get());\n".format(remap(typeArg),remap(typeArg)))      
      out.append("\tRef< %s > *result;\n".format(remapWithRef(typeArg)))
      out.append("\tDeliteCudaMalloc((void**)&result,sizeof(Ref< %s >));\n".format(remap(typeArg)))
      out.append("\tDeliteCudaMemcpyHtoDAsync((void*)result,(void*)sym_dev,sizeof(Ref< %s >));\n".format(remap(typeArg)))
      out.append("\treturn result;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "%s *sendCuda_%s(%s *sym)".format(remap(tp),mangledName(remap(tp)),remap(tp,"Host"))
      out.append(signature + " {\n")
      out.append("\t%s *sym_dev = new %s();\n".format(remap(tp),remap(tp)))
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(!isPrimitiveType(elemtp)) {
          out.append("\tsym_dev->%s = sendCuda_%s(sym->%s);\n".format(elem._1,mangledName(remap(elemtp)),elem._1))
        }
        else {
          out.append("\tsym_dev->%s = sym->%s;\n".format(elem._1,elem._1)) 
        }
      }
      out.append("\t%s result;\n".format(remapWithRef(tp)))
      out.append("\tDeliteCudaMalloc((void**)&result,sizeof(%s));\n".format(remap(tp)))
      out.append("\tDeliteCudaMemcpyHtoDAsync((void*)result,(void*)sym_dev,sizeof(%s));\n".format(remap(tp)))
      out.append("\treturn result;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(remap(tp).startsWith("DeliteArray<")) {
      remap(tp) match {
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "%s *sendCuda_%s(%s *sym)".format(remap(tp),mangledName(remap(tp)),remap(tp,"Host"))
          out.append(signature + " {\n")
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tmemcpy(hostPtr, sym->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\t%s *sym_dev = new %s(sym->length);\n".format(remap(tp),remap(tp)))
          out.append("\tDeliteCudaMemcpyHtoDAsync(sym_dev->data, hostPtr, sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\t%s result;\n".format(remapWithRef(tp)))
          out.append("\tDeliteCudaMalloc((void**)&result,sizeof(%s));\n".format(remap(tp)))
          out.append("\tDeliteCudaMemcpyHtoDAsync((void*)result,(void*)sym_dev,sizeof(%s));\n".format(remap(tp)))
          out.append("\treturn result;\n")
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => // DeliteArrayObject
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "%s *sendCuda_%s(%s *sym)".format(remap(tp),mangledName(remap(tp)),remap(tp,"Host"))
          out.append(signature + " {\n")
          out.append("\t%s *hostPtr;\n".format(remapWithRef(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remapWithRef(typeArg)))
          out.append("\tfor(int i=0; i<sym->length; i++) {\n")
          out.append("\t\thostPtr[i] = sendCuda_%s(sym->data[i]);\n".format(mangledName(remap(typeArg))))
          out.append("\t}\n")
          out.append("\t%s *sym_dev = new %s(sym->length);\n".format(remap(tp),remap(tp)))
          out.append("\tDeliteCudaMemcpyHtoDAsync(sym_dev->data, hostPtr, sym->length*sizeof(%s));\n".format(remapWithRef(typeArg)))
          out.append("\t%s result;\n".format(remapWithRef(tp)))
          out.append("\tDeliteCudaMalloc((void**)&result,sizeof(%s));\n".format(remap(tp)))
          out.append("\tDeliteCudaMemcpyHtoDAsync((void*)result,(void*)sym_dev,sizeof(%s));\n".format(remap(tp)))
          out.append("\treturn result;\n")
          out.append("}\n")
          (signature+";\n", out.toString)
      }
    }
    else {
      super.emitSendSlave(tp)
    }
  }

  override def emitRecvSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "HostRef< %s > *recvCuda_Ref__%s__(Ref< %s > *%s_dev)".format(remap(typeArg),mangledName(remap(tp)),remap(typeArg),"sym")
      out.append(signature + " {\n")
      out.append("assert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "%s *recvCuda_%s(%s *sym_dev)".format(remap(tp,"Host"),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\t%s *sym_dev_temp;\n".format(remap(tp)))
      out.append("\tDeliteCudaMallocHost((void**)&sym_dev_temp,sizeof(%s));".format(remap(tp)))
      out.append("\tDeliteCudaMemcpyDtoHAsync(sym_dev_temp, sym_dev, sizeof(%s));\n".format(remap(tp)))
      out.append("\t%s *sym = new %s();\n".format(remap(tp,"Host"),remap(tp,"Host")))
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(!isPrimitiveType(elemtp)) {
          out.append("\tsym->%s = recvCuda_%s(sym_dev_temp->%s);\n".format(elem._1,mangledName(remap(elemtp)),elem._1))
        }
        else {
          out.append("\tsym->%s = sym_dev_temp->%s;\n".format(elem._1,elem._1))
        }
      }
      out.append("\treturn sym;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(remap(tp).startsWith("DeliteArray<")) {
      remap(tp) match {
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "%s *recvCuda_%s(%s *sym_dev)".format(remap(tp,"Host"),mangledName(remap(tp)),remap(tp))
          out.append(signature + " {\n")
          out.append("\t%s *sym = new %s(0);\n".format(remap(tp,"Host"),remap(tp,"Host")))
          out.append("\tDeliteCudaMemcpyDtoHAsync(sym, sym_dev, sizeof(%s));\n".format(remap(tp)))
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, sym->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tsym = new %s(sym->length);\n".format(remap(tp,"Host")))
          out.append("\tmemcpy(sym->data,hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\treturn sym;\n")
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => 
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "%s *recvCuda_%s(%s *sym_dev)".format(remap(tp,"Host"),mangledName(remap(tp)),remap(tp))
          out.append(signature + " {\n")
          out.append("assert(false);\n")
          out.append("}\n")
          (signature+";\n", out.toString)
      }
    }
    else {
      super.emitRecvSlave(tp)
    }
  }

  /*
  override def emitSendViewSlave(sym: Sym[Any]): (String,String) = {
  }
  override def emitRecvViewSlave(sym: Sym[Any]): (String,String) = {
  }
  */

  override def emitSendUpdateSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "void sendUpdateCuda_Ref__%s__(Ref< %s > *%s_dev, HostRef< %s > *%s)".format(mangledName(remap(tp)),remap(typeArg),"sym",remap(typeArg),"sym")
      out.append(signature + " {\n")
      out.append("assert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "void sendUpdateCuda_%s(%s *%s_dev, %s *%s)".format(mangledName(remap(tp)),remap(tp),"sym",remap(tp,"Host"),"sym")
      out.append(signature + " {\n")
      out.append("assert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(remap(tp).startsWith("DeliteArray<")) {
      remap(tp) match {
       case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "void sendUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remap(tp,"Host"))
          out.append(signature + " {\n")
          out.append("\t%s *hostDA;\n".format(remap(tp)))
          out.append("\tDeliteCudaMallocHost((void**)&hostDA,sizeof(%s));\n".format(remap(tp)))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostDA, sym_dev, sizeof(%s));\n".format(remap(tp)))
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tmemcpy(hostPtr, sym->data, sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMemcpyHtoDAsync(hostDA->data, hostPtr, sym->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => 
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "void sendUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remap(tp,"Host"))
          out.append(signature + " {\n")
          out.append("assert(false);\n")
          out.append("}\n")
          (signature+";\n", out.toString)
      }
    }
    else {
      super.emitSendUpdateSlave(tp)
    }
  }

  override def emitRecvUpdateSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      val out = new StringBuilder
      val typeArg = tp.typeArguments.head
      if (!isPrimitiveType(typeArg)) throw new GenerationFailedException("emitSend Failed") //TODO: Enable non-primitie type refs
      val signature = "void recvUpdateCuda_Ref__%s__(Ref< %s > *%s_dev, HostRef< %s > *%s)".format(mangledName(remap(tp)),remap(typeArg),"sym",remap(typeArg),"sym")
      out.append(signature + " {\n")
      out.append("assert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(encounteredStructs.contains(structName(tp))) {
      val out = new StringBuilder
      val signature = "void recvUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remap(tp,"Host"))
      out.append(signature + " {\n")
      out.append("\t%s *hostStruct;\n".format(remap(tp)))
      out.append("\tDeliteCudaMallocHost((void**)&hostStruct,sizeof(%s));\n".format(remap(tp)))
      out.append("\tDeliteCudaMemcpyDtoHAsync(hostStruct, sym_dev, sizeof(%s));\n".format(remap(tp)))
      for(elem <- encounteredStructs(structName(tp))) {
        val elemtp = baseType(elem._2)
        if(isPrimitiveType(elemtp)) {
          out.append("\tsym->%s = hostStruct->%s;\n".format(elem._1,elem._1))
        }
        else { // Always assume array type?
          out.append("\trecvUpdateCuda_%s(hostStruct->%s, sym->%s);\n".format(mangledName(remap(elemtp)),elem._1,elem._1))
        }
      }
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if(remap(tp).startsWith("DeliteArray<")) {
      remap(tp) match {
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "void recvUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remap(tp,"Host"))
          out.append(signature + " {\n")
          out.append("\t%s *hostDA;\n".format(remap(tp)))
          out.append("\tDeliteCudaMallocHost((void**)&hostDA,sizeof(%s));\n".format(remap(tp)))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostDA, sym_dev, sizeof(%s));\n".format(remap(tp)))
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,hostDA->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, hostDA->data, hostDA->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("\tmemcpy(sym->data,hostPtr,hostDA->length*sizeof(%s));\n".format(remap(typeArg)))
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "void recvUpdateCuda_%s(%s *sym_dev, %s *sym)".format(mangledName(remap(tp)),remap(tp),remap(tp,"Host"))
          out.append(signature + " {\n")
          out.append("assert(false);\n")
          out.append("}\n")
          (signature+";\n", out.toString)
      }
    }
    else {
      super.emitRecvUpdateSlave(tp)
    }
  }

}
