package ppl.dsl.optila.extern

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.opencl.OpenCLGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._
import ppl.dsl.optila.OpenCLGenDataStruct

import ppl.dsl.optila.OptiLAExp

trait OptiLAOpenCLGenExternal extends OpenCLGenExternalBase with OpenCLGenDataStruct {
  val IR: OptiLAExp
  import IR._
  
  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@DenseMatrixTimesVectorBLAS(x,y) =>
      val args = scala.List("'t'", "%1$s.numCols", "%1$s.numRows", "%1$s.data", "%2$s.data", "%3$s.data")
                 .map { _.format(quote(x), quote(y), quote(sym)) }
      emitMethodCall(sym, e, clBLAS, args)
      registerKernel(scala.List(sym))

    case e@DenseMatrixMultiplyBLAS(x,y) =>
      val args = scala.List("'n'", "'n'", "%2$s.numCols", "%1$s.numRows", "%2$s.numRows", "1.0", "%2$s.data", "%2$s.numCols", "%1$s.data", "%1$s.numCols", "0.0", "%3$s.data", "%3$s.numCols")
                 .map { _.format(quote(x), quote(y), quote(sym)) }
      emitMethodCall(sym, e, clBLAS, args)
      registerKernel(scala.List(sym))

    case _ => super.emitExternalNode(sym, rhs)
  }
    
  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
      /*
    case e@DenseMatrixTimesVectorBLAS(x,y) =>
      val tp = e.mA.typeArguments.head.toString.toLowerCase
      val func = tp match {
        case "double" => "clblasDgemv"
        case "float" => "clblasSgemv"
      }
      emitInterfaceAndMethod(clBLAS, e.funcName, scala.List("char transpose", "int mat_col", "int mat_row", "cl_mem mat1", "cl_mem vec2", "cl_mem vec3"),
"""
{
  %1$s(transpose, mat_col, mat_row, 1.0, mat1, mat_col, vec2, 1, 0.0, vec3, 1);
}""".format(func))

   case e@DenseMatrixMultiplyBLAS(x,y) =>
      val tp = e.mA.typeArguments.head.toString.toLowerCase
      val func = tp match {
        case "double" => "clblasDgemm"
        case "float" => "clblasSgemm"
      }
      emitInterfaceAndMethod(clBLAS, e.funcName, scala.List("char n1", "char n2", "int mat2_col", "int mat1_row", "int mat2_row", tp+" a", "cl_mem mat2", "int mat2_col_b", "cl_mem mat1", "int mat1_col", tp+" b", "cl_mem mat3", "int mat3_col"),
"""
{
  %1$s(n1, n2, mat2_col, mat1_row, mat2_row, a, mat2, mat2_col_b, mat1, mat1_col, b, mat3, mat3_col);
}""".format(func)) 
    */
    case _ => super.emitExternalLib(rhs)
  }
}
