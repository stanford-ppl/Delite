package ppl.dsl.optila.extern

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.cpp.CGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

import ppl.dsl.optila.OptiLAExp

trait OptiLACGenExternal extends CGenExternalBase {
  val IR: OptiLAExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@DenseMatrixTimesVectorBLAS(xR,xC,x,y) =>
      val args = scala.List("%1$s->data", "%2$s->data", "%3$s->data", "%4$s", "%5$s", "0", "1")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC)) }
      emitMethodCall(sym, e, BLAS, args)

    case e@DenseMatrixMultiplyBLAS(xR,xC,x,yR,yC,y) =>
      val args = scala.List("%1$s->data", "%2$s->data", "%3$s->data", "%4$s", "%5$s", "%6$s")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC), quote(yC)) }
      emitMethodCall(sym, e, BLAS, args)

    case e@DenseMatrixSigmoidVectorized(xR,xC,x) =>
      val args = scala.List("%1$s->data", "%2$s->data", "0", "%3$s*%4$s")
                 .map { _.format(quote(x),quote(sym),quote(xR),quote(xC)) }
      emitMethodCall(sym, e, BLAS, args)

    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    /**
     * MatrixTimesVector
     */
    case e@DenseMatrixTimesVectorBLAS(xR,xC,x,y) =>
      val tp = remap(e.mA)
      val func = tp match {
        case "double" => "cblas_dgemv"
        case "float" => "cblas_sgemv"
      }
      emitInterfaceAndMethod(BLAS, e.funcName,
        scala.List("%1$s *mat1_ptr", "%1$s *vec2_ptr", "%1$s *vec3_ptr", "int mat_row", "int mat_col", "int vec_offset", "int vec_stride") map { _.format(tp) },
        "",
        """
        {
        	vec2_ptr += vec_offset;

        	%2$s(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);
        }""".format(tp.toLowerCase, func))


    /**
     * MatrixMultiply
     */
    case e@DenseMatrixMultiplyBLAS(xR,xC,x,yR,yC,y) =>
      val tp = remap(e.mA)
      val func = tp match {
        case "double" => "cblas_dgemm"
        case "float" => "cblas_sgemm"
      }
      emitInterfaceAndMethod(BLAS, e.funcName,
        scala.List("%1$s *mat1_ptr", "%1$s *mat2_ptr", "%1$s *mat3_ptr", "int mat1_r", "int mat1_c", "int mat2_c") map { _.format(tp) },
        "",
        """
        {
        	%2$s(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);
        }""".format(tp.toLowerCase, func))


    /**
     * MatrixSigmoid
     */
    case e@DenseMatrixSigmoidVectorized(xR,xC,x) =>
      val tp = remap(e.mA)
      val func = tp match {
        case "double" => "exp"
        case "float" => "expf"
      }
      // TODO: should be just pure c++, not BLAS lib
      emitInterfaceAndMethod(BLAS, e.funcName,
        scala.List("%1$s *vec1_ptr", "%1$s *vec2_ptr", "int start", "int end") map { _.format(tp) },
        "",
        """
        {
      	  int i = 0;
        	for(i=start; i<end; i++) {
        		vec2_ptr[i] = 1.0 / (1.0+%2$s(-1.0*vec1_ptr[i]));
        	}

        }""".format(tp.toLowerCase, func))

    case _ => super.emitExternalLib(rhs)
  }
}
