package ppl.dsl.optila.extern

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

import ppl.dsl.optila.OptiLAExp

trait OptiLAScalaGenExternal extends ScalaGenExternalBase {
  val IR: OptiLAExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@DenseMatrixTimesVectorBLAS(xR,xC,x,y) =>
      val args = scala.List("%1$s", "%2$s", "%3$s", "%4$s", "%5$s", "0", "1")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC)) }
      emitMethodCall(sym, e, BLAS, args)

    case e@DenseMatrixMultiplyBLAS(xR,xC,x,yR,yC,y) =>
      val args = scala.List("%1$s", "%2$s", "%3$s", "%4$s", "%5$s", "%6$s")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC), quote(yC)) }
      emitMethodCall(sym, e, BLAS, args)

    case e@DenseMatrixSigmoidVectorized(xR,xC,x) =>
      val args = scala.List("%1$s", "%2$s", "0", "%3$s*%4$s")
                 .map { _.format(quote(x),quote(sym),quote(xR),quote(xC)) }
      emitMethodCall(sym, e, BLAS, args)

    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    /**
     * MatrixTimesVector
     */
    case e@DenseMatrixTimesVectorBLAS(xR,xC,x,y) =>
      val tp = e.mA.toString
      val func = tp match {
        case "Double" => "cblas_dgemv"
        case "Float" => "cblas_sgemv"
      }
      emitInterfaceAndMethod(BLAS, e.funcName,
        scala.List("mat1:Array[%1$s]", "vec2:Array[%1$s]", "vec3:Array[%1$s]", "mat_row:Int", "mat_col:Int", "vec_offset:Int", "vec_stride:Int") map { _.format(tp) },
        scala.List("j%1$sArray mat1", "j%1$sArray vec2", "j%1$sArray vec3", "jint mat_row", "jint mat_col", "jint vec_offset", "jint vec_stride") map { _.format(tp.toLowerCase) },
        """
        {
        	jboolean copy;

        	j%1$s *mat1_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)mat1, &copy));
        	j%1$s *vec2_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)vec2, &copy));
        	j%1$s *vec3_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)vec3, &copy));

        	vec2_ptr += vec_offset;

        	%2$s(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

        	env->ReleasePrimitiveArrayCritical(mat1, mat1_ptr, 0);
        	env->ReleasePrimitiveArrayCritical(vec2, vec2_ptr, 0);
        	env->ReleasePrimitiveArrayCritical(vec3, vec3_ptr, 0);
        }""".format(tp.toLowerCase, func))


    /**
     * MatrixMultiply
     */
    case e@DenseMatrixMultiplyBLAS(xR,xC,x,yR,yC,y) =>
      val tp = e.mA.toString
      val func = tp match {
        case "Double" => "cblas_dgemm"
        case "Float" => "cblas_sgemm"
      }
      emitInterfaceAndMethod(BLAS, e.funcName,
        scala.List("mat1:Array[%1$s]", "mat2:Array[%1$s]", "mat3:Array[%1$s]", "mat1_r:Int", "mat1_c:Int", "mat2_c:Int") map { _.format(tp) },
        scala.List("j%1$sArray mat1", "j%1$sArray mat2", "j%1$sArray mat3", "jint mat1_r", "jint mat1_c", "jint mat2_c") map { _.format(tp.toLowerCase) },
        """
        {
        	jboolean copy;
        	j%1$s *mat1_ptr = (j%1$s*)env->GetPrimitiveArrayCritical((jarray)mat1, &copy);
        	j%1$s *mat2_ptr = (j%1$s*)env->GetPrimitiveArrayCritical((jarray)mat2, &copy);
        	j%1$s *mat3_ptr = (j%1$s*)env->GetPrimitiveArrayCritical((jarray)mat3, &copy);

        	%2$s(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

        	env->ReleasePrimitiveArrayCritical(mat1, mat1_ptr, 0);
        	env->ReleasePrimitiveArrayCritical(mat2, mat2_ptr, 0);
        	env->ReleasePrimitiveArrayCritical(mat3, mat3_ptr, 0);
        }""".format(tp.toLowerCase, func))


    /**
     * MatrixSigmoid
     */
    case e@DenseMatrixSigmoidVectorized(xR,xC,x) =>
      val tp = e.mA.toString
      val func = tp match {
        case "Double" => "exp"
        case "Float" => "expf"
      }
      // TODO: should be just pure c++, not BLAS lib
      emitInterfaceAndMethod(BLAS, e.funcName,
        scala.List("vec1:Array[%1$s]", "vec2:Array[%1$s]", "start:Int", "end:Int") map { _.format(tp) },
        scala.List("j%1$sArray vec1", "j%1$sArray vec2", "jint start", "jint end") map { _.format(tp.toLowerCase) },
        """
        {
      	  int i = 0;
        	jboolean copy;

        	j%1$s *vec1_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)vec1, &copy));
        	j%1$s *vec2_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)vec2, &copy));

        	for(i=start; i<end; i++) {
        		vec2_ptr[i] = 1.0 / (1.0+%2$s(-1.0*vec1_ptr[i]));
        	}

        	env->ReleasePrimitiveArrayCritical(vec1, vec1_ptr, 0);
        	env->ReleasePrimitiveArrayCritical(vec2, vec2_ptr, 0);
        }""".format(tp.toLowerCase, func))

    case _ => super.emitExternalLib(rhs)
  }
}
