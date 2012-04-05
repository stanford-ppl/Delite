package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.virtualization.lms.common.{VariablesExp, Variables, CGenBase, CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import ppl.dsl.optiml._

trait GrayscaleImageOps extends Variables {
  this: OptiML =>

  object GrayscaleImage {
    def apply(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = grayscaleimage_obj_new(numRows, numCols)
    def apply(x: Rep[DenseMatrix[Int]])(implicit ctx: SourceContext) = grayscaleimage_obj_frommat(x)
    def cartToPolar(x: Rep[DenseMatrix[Float]], y: Rep[DenseMatrix[Float]])(implicit ctx: SourceContext) = grayscaleimage_obj_carttopolar(x,y)

//    val scharrYkernel = Matrix(Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
//    val scharrXkernel = scharrYkernel.t
  }

  implicit def repGrayscaleImageToGrayscaleImageOps[A:Manifest](x: Rep[GrayscaleImage]) = new grayscaleImageOpsCls(x)
  implicit def varToGrayscaleImageOps[A:Manifest](x: Var[GrayscaleImage]) = new grayscaleImageOpsCls(readVar(x))

  class grayscaleImageOpsCls(x: Rep[GrayscaleImage]) {
    import GrayscaleImage._

    def bitwiseOrDownsample()(implicit ctx: SourceContext) = GrayscaleImage(x.downsample(unit(2),unit(2)) { slice => slice(unit(0),unit(0)) | slice(unit(1),unit(0)) | slice(unit(0),unit(1)) | slice(unit(1),unit(1)) })
    def gradients(polar: Rep[Boolean] = unit(false))(implicit ctx: SourceContext) = { // unroll at call site for parallelism (temporary until we have composite op)
      val scharrYkernel = DenseMatrix[Int](unit(3), unit(3))
      scharrYkernel(unit(0),unit(0)) = unit(-3); scharrYkernel(unit(0),unit(1)) = unit(-10); scharrYkernel(unit(0),unit(2)) = unit(-3)
      scharrYkernel(unit(2),unit(0)) = unit(3); scharrYkernel(unit(2),unit(1)) = unit(10); scharrYkernel(unit(2),unit(2)) = unit(3)
      val scharrXkernel = scharrYkernel.t
      val a = x.convolve(scharrXkernel)
      val b = x.convolve(scharrYkernel)
      if (polar) cartToPolar(a.toFloat,b.toFloat) else (a.toFloat,b.toFloat)
    }
    // TODO: need to refactor using CanBuildFrom and 2.8 techniques to avoid this duplication.
    //def convolve(kernel: Rep[Matrix[Int]]) = GrayscaleImage(x.windowedFilter(kernel.numRows, kernel.numCols) { slice => (slice *:* kernel).sum })
    def windowedFilter(rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[DenseMatrix[Int]] => Rep[Int])(implicit ctx: SourceContext) =
      GrayscaleImage(image_windowed_filter(x,rowDim, colDim, block))
  }

  // object defs
  def grayscaleimage_obj_new(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_frommat(x: Rep[DenseMatrix[Int]])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_carttopolar(x: Rep[DenseMatrix[Float]], y: Rep[DenseMatrix[Float]])(implicit ctx: SourceContext): (Rep[DenseMatrix[Float]],Rep[DenseMatrix[Float]])
}


trait GrayscaleImageOpsExp extends GrayscaleImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GrayscaleImageObjectNew(numRows: Exp[Int], numCols: Exp[Int]) extends Def[GrayscaleImage]
  case class GrayscaleImageObjectFromMat(x: Exp[DenseMatrix[Int]]) extends Def[GrayscaleImage]


  ////////////////////////////////
  // implemented via delite ops

  case class GrayscaleImageObjectCartToPolarMagnitude(x: Exp[DenseMatrix[Float]], y: Exp[DenseMatrix[Float]])
    extends MatrixArithmeticZipWith[Float,DenseMatrix[Float]] {

    val intfA = denseMatToInterface(x)
    val intfB = denseMatToInterface(y)    
    def func = (a,b) => sqrt(a*a + b*b).AsInstanceOf[Float]
  }

  case class GrayscaleImageObjectCartToPolarPhase(x: Exp[DenseMatrix[Float]], y: Exp[DenseMatrix[Float]])
    extends MatrixArithmeticZipWith[Float,DenseMatrix[Float]] {
      
    val intfA = denseMatToInterface(x)
    val intfB = denseMatToInterface(y)      
    def func = (a,b) => (atan2(b, a)*unit(180)/Pi).AsInstanceOf[Float]
  }

  ////////////////////
  // object interface

  def grayscaleimage_obj_new(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(GrayscaleImageObjectNew(numRows, numCols))
  def grayscaleimage_obj_frommat(x: Exp[DenseMatrix[Int]])(implicit ctx: SourceContext) = reflectEffect(GrayscaleImageObjectFromMat(x))
  def grayscaleimage_obj_carttopolar(x: Exp[DenseMatrix[Float]], y: Exp[DenseMatrix[Float]])(implicit ctx: SourceContext) = {
    val mag = reflectPure(GrayscaleImageObjectCartToPolarMagnitude(x,y))
    val phase = reflectPure(GrayscaleImageObjectCartToPolarPhase(x,y)) map { a => if (a < unit(0f)) a + unit(360f) else a } 
    (mag,phase)
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GrayscaleImageObjectCartToPolarPhase(a,b) => reflectPure(new { override val original = Some(f,e) } with GrayscaleImageObjectCartToPolarPhase(f(a),f(b)))(mtype(manifest[A]), implicitly[SourceContext])
    case Reflect(GrayscaleImageObjectFromMat(x), u, es) => reflectMirrored(Reflect(GrayscaleImageObjectFromMat(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}


trait ScalaGenGrayscaleImageOps extends ScalaGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // case GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new generated.scala.GrayscaleImageImpl(" + quote(numRows) + "," + quote(numCols) + ")")
    // case GrayscaleImageObjectFromMat(m) => emitValDef(sym, "new generated.scala.GrayscaleImageImpl(" + quote(m) + ")")
    case GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new generated.scala.Image[Int](" + quote(numRows) + "," + quote(numCols) + ")")
    case GrayscaleImageObjectFromMat(m) => emitValDef(sym, "new generated.scala.Image[Int](" + quote(m) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenGrayscaleImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: GrayscaleImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenGrayscaleImageOps extends CGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
