package ppl.delite.framework.datastructures

import scala.virtualization.lms.internal.{GenerationFailedException, Expressions, CudaDeviceTransfer, CudaCodegen}


trait DeliteCudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen =>

  val IR: Expressions
  import IR._

  override def emitSendSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      throw new GenerationFailedException("CudaDeviceTransfer: Ref types are not supported yet.")
    }
    else {
      remap(tp) match {
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "%s *sendCuda_%s(Host%s *%s)".format(remap(tp),mangledName(remap(tp)),remap(tp),"sym")
          out.append(signature + " {\n")
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format("sym",remap(typeArg)))
          out.append("\tmemcpy(hostPtr, %s->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("\t%s *%s_dev = new %s(%s->length);\n".format(remap(tp),"sym",remap(tp),"sym"))
          out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("\treturn %s_dev;\n".format("sym"))
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => super.emitSendSlave(tp)
      }
    }
  }

  override def emitRecvSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      throw new GenerationFailedException("CudaDeviceTransfer: Ref types are not supported yet.")
    }
    else {
      remap(tp) match {
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "Host%s *recvCuda_%s(%s *%s_dev)".format(remap(tp),mangledName(remap(tp)),remap(tp),"sym")
          out.append(signature + " {\n")
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s_dev->length*sizeof(%s));\n".format("sym",remap(typeArg)))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s_dev->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("\tHost%s *%s = new Host%s(%s_dev->length);\n".format(remap(tp),"sym",remap(tp),"sym","sym"))
          out.append("\tmemcpy(%s->data,hostPtr,%s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("\treturn %s;\n".format("sym"))
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => super.emitRecvSlave(tp)
      }
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
      throw new GenerationFailedException("CudaDeviceTransfer: Ref types are not supported yet.")
    }
    else {
      remap(tp) match {
       case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "void sendUpdateCuda_%s(%s *%s_dev, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym",remap(tp),"sym")
          out.append(signature + " {\n")
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format("sym",remap(typeArg)))
          out.append("\tmemcpy(hostPtr, %s->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => super.emitSendUpdateSlave(tp)
      }
    }
  }

  override def emitRecvUpdateSlave(tp: Manifest[Any]): (String,String) = {
    if (tp.erasure == classOf[Variable[AnyVal]]) {
      throw new GenerationFailedException("CudaDeviceTransfer: Ref types are not supported yet.")
    }
    else {
      remap(tp) match {
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" =>
          val out = new StringBuilder
          val typeArg = tp.typeArguments.head
          val signature = "void recvUpdateCuda_%s(%s *%s_dev, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym",remap(tp),"sym")
          out.append(signature + " {\n")
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format("sym",remap(typeArg)))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("\tmemcpy(%s->data,hostPtr,%s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
          out.append("}\n")
          (signature+";\n", out.toString)
        case _ => super.emitRecvUpdateSlave(tp)
      }
    }
  }

}
