package ppl.dsl.optila

import virtualization.lms.internal.{Expressions, CudaDeviceTransfer, CudaCodegen}

trait OptiLACudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen =>

  val IR: Expressions
  import IR._

  override def emitSendSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "%s *sendCuda_%s(Host%s *%s)".format(remap(tp),mangledName(remap(tp)),remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format("sym",remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\t%s *%s_dev = new %s(%s->length,%s->isRow);\n".format(remap(tp),"sym",remap(tp),"sym","sym"))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\treturn %s_dev;\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "%s *sendCuda_%s(Host%s *%s)".format(remap(tp),mangledName(remap(tp)),remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("\t%s *%s_dev = new %s(%s->numRows,%s->numCols);\n".format(remap(tp),"sym",remap(tp),"sym","sym"))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("\treturn %s_dev;\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitSendSlave(tp)
    }
  }

  override def emitRecvSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "Host%s *recvCuda_%s(%s *%s_dev)".format(remap(tp),mangledName(remap(tp)),remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s_dev->length*sizeof(%s));\n".format("sym",remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s_dev->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\tHost%s *%s = new Host%s(%s_dev->length,%s_dev->isRow);\n".format(remap(tp),"sym",remap(tp),"sym","sym"))
        out.append("\tmemcpy(%s->data,hostPtr,%s->length*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\treturn %s;\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "Host%s *recvCuda_%s(%s *%s_dev)".format(remap(tp),mangledName(remap(tp)),remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s_dev->numRows*%s_dev->numCols*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s_dev->numRows*%s_dev->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("\tHost%s *%s = new Host%s(%s_dev->numRows,%s_dev->numCols);\n".format(remap(tp),"sym",remap(tp),"sym","sym"))
        out.append("\tmemcpy(%s->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("\treturn %s;\n".format("sym"))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitRecvSlave(tp)
    }
  }

  //TODO: Implement view for slave devices
  /*
  override def emitSendViewSlave(sym: Sym[Any]): (String,String) = {
  }

  override def emitRecvViewSlave(sym: Sym[Any]): (String,String) = {
  }
  */

  override def emitSendUpdateSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
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
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void sendUpdateCuda_%s(%s *%s_dev, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym",remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitSendUpdateSlave(tp)
    }
  }

  override def emitRecvUpdateSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
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
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = tp.typeArguments.head
        val signature = "void recvUpdateCuda_%s(%s *%s_dev, Host%s *%s)".format(mangledName(remap(tp)),remap(tp),"sym",remap(tp),"sym")
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym",remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("\tmemcpy(%s->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format("sym","sym","sym",remap(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitRecvUpdateSlave(tp)
    }
  }

}
