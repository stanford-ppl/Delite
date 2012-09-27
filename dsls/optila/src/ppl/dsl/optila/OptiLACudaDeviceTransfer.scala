package ppl.dsl.optila

import virtualization.lms.internal.{Expressions, CudaDeviceTransfer}

trait OptiLACudaDeviceTransfer extends CudaDeviceTransfer {

  val IR: Expressions
  import IR._

  override def emitSendSlave(sym: Sym[Any]): (String,String) = {
    remap(sym.tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "%s *sendCuda_%s(%s *%s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\t%s *%s_dev = new %s(%s->length,%s->isRow);\n".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym),quote(sym)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\treturn %s_dev;\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "%s *sendCuda_%s(%s *%s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("\t%s *%s_dev = new %s(%s->numRows,%s->numCols);\n".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym),quote(sym)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("\treturn %s_dev;\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitSendSlave(sym)
    }
  }

  override def emitRecvSlave(sym: Sym[Any]): (String,String) = {
    remap(sym.tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "%s *recvCuda_%s(%s *%s_dev)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\t%s *%s = new %s(%s_dev->length,%s_dev->isRow);\n".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym),quote(sym)))
        out.append("\tmemcpy(%s->data,hostPtr,%s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "%s *recvCuda_%s(%s *%s_dev)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("\t%s *%s = new %s(%s_dev->numRows,%s_dev->numCols);\n".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym),quote(sym)))
        out.append("\tmemcpy(%s->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitRecvSlave(sym)
    }
  }

  //TODO: Implement view for slave devices
  /*
  override def emitSendViewSlave(sym: Sym[Any]): (String,String) = {
  }

  override def emitRecvViewSlave(sym: Sym[Any]): (String,String) = {
  }
  */

  override def emitSendUpdateSlave(sym: Sym[Any]): (String,String) = {
    remap(sym.tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "void sendUpdateCuda_%s(%s *%s_dev, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "void sendUpdateCuda_%s(%s *%s_dev, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\tmemcpy(hostPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s_dev->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitSendUpdateSlave(sym)
    }
  }

  override def emitRecvUpdateSlave(sym: Sym[Any]): (String,String) = {
    remap(sym.tp) match {
      case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "void recvUpdateCuda_%s(%s *%s_dev, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->length*sizeof(%s));\n".format(quote(sym),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\tmemcpy(%s->data,hostPtr,%s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val signature = "void recvUpdateCuda_%s(%s *%s_dev, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
        out.append(signature + " {\n")
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s_dev->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("\tmemcpy(%s->data, hostPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
        out.append("}\n")
        (signature+";\n", out.toString)
      case _ => super.emitSendUpdateSlave(sym)
    }
  }

}
