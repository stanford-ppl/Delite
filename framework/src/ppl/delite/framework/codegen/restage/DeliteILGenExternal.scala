package ppl.delite.framework.codegen.restage

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._
import ppl.delite.framework.DeliteILExp

trait DeliteILScalaGenExternal extends ScalaGenExternalBase {
  val IR: DeliteILExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e:DeliteOpExternal[_] =>
      // hack alert - copied from OptiLAScalaGenExternal
      // see DeliteCodeGenRestage.scala for more comments about what we ought to be doing here
      e.funcName match {
        case "matMult" =>
          val args = scala.List("%1$s._data", "%2$s._data", "%3$s._data", "%1$s._numRows", "%1$s._numCols", "%2$s._numCols")
                     .map { _.format(quote(e.inputs(0)), quote(e.inputs(1)), quote(sym)) }
          emitMethodCall(sym, e, LAPACK, args)

        case "matMultV" =>
          val args = scala.List("%1$s._data", "%2$s._data", "%3$s._data", "%1$s._numRows", "%1$s._numCols", "0", "1")
                     .map { _.format(quote(e.inputs(0)), quote(e.inputs(1)), quote(sym)) }
          emitMethodCall(sym, e, LAPACK, args)

        case _ => throw new IllegalArgumentException("tried to emit unknown Delite IL external node")
      }

    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e:DeliteOpExternal[_] =>
      e.funcName match {
        case "matMult" =>
          // val tp = e.mA.toString
          val tp = manifest[Double].toString
          val func = tp match {
            case "Double" => "cblas_dgemm"
            case "Float" => "cblas_sgemm"
          }
          emitInterfaceAndMethod(LAPACK, e.funcName,
            scala.List("mat1:Array[%1$s]", "mat2:Array[%1$s]", "mat3:Array[%1$s]", "mat1_r:Int", "mat1_c:Int", "mat2_c:Int") map { _.format(tp) },
            scala.List("j%1$sArray mat1", "j%1$sArray mat2", "j%1$sArray mat3", "jint mat1_r", "jint mat1_c", "jint mat2_c") map { _.format(tp.toLowerCase) },
            """
            {
            	jboolean copy;
            	j%1$s *mat1_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy);
            	j%1$s *mat2_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy);
            	j%1$s *mat3_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy);

            	%2$s(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

            	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
            	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
            	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
            }""".format(tp.toLowerCase, func))

        case "matMultV" =>
          // val tp = e.mA.toString
          val tp = manifest[Double].toString
          val func = tp match {
            case "Double" => "cblas_dgemv"
            case "Float" => "cblas_sgemv"
          }
          emitInterfaceAndMethod(LAPACK, e.funcName,
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

        case _ => throw new IllegalArgumentException("tried to emit unknown Delite IL external lib")
      }

    case _ => super.emitExternalLib(rhs)
  }
}
