package ppl.dsl.optiml.extern

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

import ppl.dsl.optiml.OptiMLExp

trait OptiMLScalaGenExternal extends ScalaGenExternalBase {
  val IR: OptiMLExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@MatrixTimesVectorBLAS(x,y) =>
      val lib = BLAS
      val args = scala.List("%1$s.data", "%2$s.data", "%3$s.data", "%1$s.numRows", "%1$s.numCols", "0", "1") 
                 .map { _.format(quote(getBlockResult(x)), quote(getBlockResult(y)), quote(getBlockResult(e.allocVal))) }
      emitMethodCall(sym, e, lib, args)
      
    case _ => super.emitNode(sym, rhs)
  }
    
  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e@MatrixTimesVectorBLAS(x,y) =>
      val lib = BLAS
      val tp = e.mV.typeArguments.head.toString
      val func = tp match {
        case "Double" => "cblas_dgemv"
        case "Float" => "cblas_sgemv"
      }
      emitInterfaceAndMethod(lib, e.funcName, 
        scala.List("mat1:Array[%1$s]", "vec2:Array[%1$s]", "vec3:Array[%1$s]", "mat_row:Int", "mat_col:Int", "vec_offset:Int", "vec_stride:Int") map { _.format(tp) },
        scala.List("j%1$sArray mat1", "j%1$sArray vec2", "j%1$sArray vec3", "jint mat_row", "jint mat_col", "jint vec_offset", "jint vec_stride") map { _.format(tp.toLowerCase) },
        """
        {
        	jboolean copy;

        	j%1$s *mat1_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
        	j%1$s *vec2_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
        	j%1$s *vec3_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

        	vec2_ptr += vec_offset;

        	%2$s(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

        	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
        	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
        	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
        }""".format(tp.toLowerCase, func))
      
    case _ => super.emitExternalLib(rhs)
  }
}