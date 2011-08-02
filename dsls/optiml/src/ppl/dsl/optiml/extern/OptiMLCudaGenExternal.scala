package ppl.dsl.optiml.extern

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.cuda.CudaGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._
import ppl.dsl.optiml.datastruct.CudaGenDataStruct

import ppl.dsl.optiml.OptiMLExp

trait OptiMLCudaGenExternal extends CudaGenExternalBase with CudaGenDataStruct {
  val IR: OptiMLExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@MatrixTimesVectorBLAS(x,y) =>
      val lib = cuBLAS
      val args = scala.List("'t'", "%1$s.numCols", "%1$s.numRows", "%1$s.data", "%2$s.data", "%3$s.data")
                 .map { _.format(quote(getBlockResult(x)), quote(getBlockResult(y)), quote(sym)) }
      emitMethodCall(e, lib, args)
      emitVectorAlloc(sym,"%s.numRows".format(quote(getBlockResult(x))),"false",false)

    case _ => super.emitNode(sym, rhs)
  }
    
  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e@MatrixTimesVectorBLAS(x,y) =>
      val lib = cuBLAS
      val tp = e.mV.typeArguments.head.toString.toLowerCase
      val func = tp match {
        case "double" => "cublasDgemv"
        case "float" => "cublasSgemv"
      }
      emitInterfaceAndMethod(lib, e.funcName, scala.List("char transpose", "int mat_col", "int mat_row", "%1$s* mat1".format(tp), "%1$s* vec2".format(tp), "%1$s* vec3".format(tp)),
"""
{
  // HJ TODO: use a real stream
  //cublasSetKernelStream(stream);
  %1$s('t', mat_col, mat_row, 1.0, mat1, mat_col, vec2, 1, 0.0, vec3, 1);
}""".format(func))

    case _ => super.emitExternalLib(rhs)
  }
}
