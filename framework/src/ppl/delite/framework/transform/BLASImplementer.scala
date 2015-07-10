package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._
import scala.reflect.{SourceContext, RefinedManifest}

import ppl.delite.framework.Config
import ppl.delite.framework.ops._ 
import ppl.delite.framework.datastructures._
import ppl.delite.framework.analysis.LayoutMetadata
import ppl.delite.framework.visit._
import ppl.delite.framework.Util._

import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.delite.framework.extern.codegen.cuda.CudaGenExternalBase
import ppl.delite.framework.extern.codegen.cpp.CGenExternalBase
import ppl.delite.framework.extern.lib.{BLAS, cuBLAS}

trait BLASImplExp extends FlatArrayImplExp { self: DeliteOpsExp with DeliteFileReaderOpsExp => 
  override val implementer : BLASImplementer

  def MIN_SIZE = unit(100)

  case class Native_matMult[T:Manifest](xR: Rep[Int], xC: Rep[Int], x: Rep[DeliteArray[T]], yR: Rep[Int], yC: Rep[Int], y: Rep[DeliteArray[T]])(implicit ctx: SourceContext) extends DeliteOpExternal[DeliteArray[T]] {
    val mT = implicitly[Manifest[T]]

    override def inputs = scala.List(xR,xC,x,yR,yC,y)
    def alloc = DeliteArray[T](delite_int_times(xR, yC))
    val funcName = "matMult"
  }

  case class Native_matTimesVec[T:Manifest](xR: Rep[Int], xC: Rep[Int], x: Rep[DeliteArray[T]],y: Rep[DeliteArray[T]])(implicit ctx: SourceContext) extends DeliteOpExternal[DeliteArray[T]] {
    val mT = implicitly[Manifest[T]]

    override def inputs = scala.List(xR,xC,x,y)
    def alloc = DeliteArray[T](xR)
    val funcName = "matTimesVec"
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) => reflectPure(new { override val original = Some(f,e) } with Native_matMult(f(xR),f(xC),f(x),f(yR),f(yC),f(y))(e.mT,pos))(mtype(manifest[A]),pos)
    case e@Native_matTimesVec(xR,xC,x,y) => reflectPure(new { override val original = Some(f,e) } with Native_matTimesVec(f(xR),f(xC),f(x),f(y))(e.mT,pos))(mtype(manifest[A]),pos)

    case Reflect(e@Native_matMult(xR,xC,x,yR,yC,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_matMult(f(xR),f(xC),f(x),f(yR),f(yC),f(y))(e.mT,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@Native_matTimesVec(xR,xC,x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_matTimesVec(f(xR),f(xC),f(x),f(y))(e.mT,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  trait BLASImplementer extends FlatArrayImplementer {
    val IR: self.type

    override def implementMatrixMultiply[A:Manifest](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]], default: Block[DeliteArray2D[A]], rM1: Sym[DeliteArray2D[A]], rM2: Sym[DeliteArray2D[A]])(implicit ctx: SourceContext): Exp[Any] = (layout(lhs),layout(rhs)) match {
      case (FlatLayout(2, Plain), FlatLayout(2, Plain)) if Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float]) =>
        //if (delite_greater_than(implementSize(lhs), MIN_SIZE) && delite_greater_than(implementSize(rhs), MIN_SIZE)) {
        val lhsDims = getDims(lhs)
        val rhsDims = getDims(rhs)
        val lhsData = lhs.asFlatArrayPlain.data
        val rhsData = rhs.asFlatArrayPlain.data
        val data = reflectPure(Native_matMult(lhsDims(0), lhsDims(1), lhsData, rhsDims(0), rhsDims(1), rhsData)).withData(FlatLayout(1,Plain))
        flatarray_plain_new(data, Seq(lhsDims(0), rhsDims(1)))
        //}
        //else {
        //  super.implementMatrixMultiply(lhs, rhs)
        //}

      case _ => super.implementMatrixMultiply(lhs,rhs,default,rM1,rM2) 
    }
    override def implementMatrixVectorMultiply[A:Manifest](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]], default: Block[DeliteArray1D[A]], rM: Sym[DeliteArray2D[A]], rV: Sym[DeliteArray1D[A]])(implicit ctx: SourceContext): Exp[Any] = (layout(mat), layout(vec)) match {
      case (FlatLayout(2, Plain), FlatLayout(1, Plain)) if Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float]) =>
        val matDims = getDims(mat)
        val matData = mat.asFlatArrayPlain.data
        val vecData = vec.asDeliteArray
        reflectPure(Native_matTimesVec(matDims(0), matDims(1), matData, vecData)).withData(FlatLayout(1,Plain))

      case _ => super.implementMatrixVectorMultiply(mat,vec,default,rM,rV)
    }

  }
}

// (This TODO may no longer be relevant, unless we want different BLAS calls added to Forge)
// TODO: 'BLAS' is a Delite object right now, but to use it in the library impl, this needs to be refactored.
// much of this is boilerplate that we can generate. what do we really need?
//   -- type signature of lib call (Array*,Array*,Array*,int,int,int)
//   -- actual invocation method
//   -- mapping between op inputs and c call arguments?
trait ScalaGenBLASImpl extends ScalaGenExternalBase {
  val IR: BLASImplExp with DeliteOpsExp
  import IR._

  /**
   * JNI implementation
   */
  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val args = scala.List("%1$s", "%2$s", "%3$s", "%4$s", "%5$s", "%6$s")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC), quote(yC)) }
      emitMethodCall(sym, e, BLAS, args)

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val args = scala.List("%1$s", "%2$s", "%3$s", "%4$s", "%5$s", "0", "1")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC)) }
      emitMethodCall(sym, e, BLAS, args)
    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {

    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val tp = e.mT.toString
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
          j%1$s *mat1_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)mat1, &copy));
          j%1$s *mat2_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)mat2, &copy));
          j%1$s *mat3_ptr = (j%1$s*)(env->GetPrimitiveArrayCritical((jarray)mat3, &copy));

          %2$s(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

          env->ReleasePrimitiveArrayCritical(mat1, mat1_ptr, 0);
          env->ReleasePrimitiveArrayCritical(mat2, mat2_ptr, 0);
          env->ReleasePrimitiveArrayCritical(mat3, mat3_ptr, 0);
        }""".format(tp.toLowerCase, func))


    case e@Native_matTimesVec(xR,xC,x,y) =>
      val tp = e.mT.toString
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

    case _ => super.emitExternalLib(rhs)
  }
}

trait CudaGenBLASImpl extends CudaGenExternalBase {
  val IR: BLASImplExp with DeliteOpsExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val args = scala.List("'n'", "'n'", "%1$s", "%2$s", "%3$s", "1.0", "%4$s->data", "%1$s", "%5$s->data", "%6$s", "0.0", "%7$s->data", "%1$s")
                 .map { _.format(quote(yC), quote(xR), quote(yR), quote(y), quote(x), quote(xC), quote(sym)) }
      emitMethodCall(sym, e, cuBLAS, args)
      registerKernel(scala.List(sym))

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val args = scala.List("'t'", "%1$s", "%2$s", "%3$s->data", "%4$s->data", "%5$s->data")
                 .map { _.format(quote(xC), quote(xR), quote(x), quote(y), quote(sym)) }
      emitMethodCall(sym, e, cuBLAS, args)
      registerKernel(scala.List(sym))


    case _ => super.emitExternalNode(sym, rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val tp = remap(e.mT)
      val func = tp match {
        case "double" => "cublasDgemm"
        case "float" => "cublasSgemm"
      }
      emitInterfaceAndMethod(cuBLAS, e.funcName, scala.List("char n1", "char n2", "int mat2_col", "int mat1_row", "int mat2_row", tp+" a", "%1$s* mat2".format(tp), "int mat2_col_b", "%1$s* mat1".format(tp), "int mat1_col", tp+" b", "%1$s* mat3".format(tp), "int mat3_col"), "",
"""
{
  // HJ TODO: use a real stream
  //cublasSetKernelStream(stream);
  %1$s(n1, n2, mat2_col, mat1_row, mat2_row, a, mat2, mat2_col_b, mat1, mat1_col, b, mat3, mat3_col);
}""".format(func))

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val tp = remap(e.mT)
      val func = tp match {
        case "double" => "cublasDgemv"
        case "float" => "cublasSgemv"
      }
      emitInterfaceAndMethod(cuBLAS, e.funcName, scala.List("char transpose", "int mat_col", "int mat_row", "%1$s* mat1".format(tp), "%1$s* vec2".format(tp), "%1$s* vec3".format(tp)), "",
"""
{
  // HJ TODO: use a real stream
  //cublasSetKernelStream(stream);
  %1$s(transpose, mat_col, mat_row, 1.0, mat1, mat_col, vec2, 1, 0.0, vec3, 1);
}""".format(func))


    case _ => super.emitExternalLib(rhs)
  }
}

trait OpenCLGenBLASImpl // todo: implement

trait CGenBLASImpl extends CGenExternalBase {
  val IR: BLASImplExp with DeliteOpsExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val args = scala.List("%1$s->data", "%2$s->data", "%3$s->data", "%4$s", "%5$s", "%6$s")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC), quote(yC)) }
      emitMethodCall(sym, e, BLAS, args)

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val args = scala.List("%1$s->data", "%2$s->data", "%3$s->data", "%4$s", "%5$s", "0", "1")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC)) }
      emitMethodCall(sym, e, BLAS, args)

    case _ => super.emitExternalNode(sym, rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val tp = remap(e.mT)
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

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val tp = remap(e.mT)
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

    case _ => super.emitExternalLib(rhs)
  }
}
