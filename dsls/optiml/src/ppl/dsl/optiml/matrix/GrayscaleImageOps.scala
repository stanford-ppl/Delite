package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.virtualization.lms.common.{VariablesExp, Variables, CGenBase, CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import ppl.dsl.optiml.CudaGenDataStruct
import ppl.dsl.optiml.{Vector, Matrix, GrayscaleImage}
import ppl.dsl.optiml.{OptiML, OptiMLExp}

trait GrayscaleImageOps extends Variables {
  this: OptiML =>

  object GrayscaleImage {
    def apply(numRows: Rep[Int], numCols: Rep[Int]) = grayscaleimage_obj_new(numRows, numCols)
    def apply(x: Rep[Matrix[Int]]) = grayscaleimage_obj_frommat(x)
    def cartToPolar(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]]) = grayscaleimage_obj_carttopolar(x,y)

//    val scharrYkernel = Matrix(Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
//    val scharrXkernel = scharrYkernel.t
  }

  implicit def repGrayscaleImageToGrayscaleImageOps[A:Manifest](x: Rep[GrayscaleImage]) = new grayscaleImageOpsCls(x)
  implicit def varToGrayscaleImageOps[A:Manifest](x: Var[GrayscaleImage]) = new grayscaleImageOpsCls(readVar(x))

  class grayscaleImageOpsCls(x: Rep[GrayscaleImage]) {
    import GrayscaleImage._

    def bitwiseOrDownsample() = GrayscaleImage(x.downsample(2,2) { slice => slice(0,0) | slice(1,0) | slice(0,1) | slice(1,1) })
    def gradients(polar: Rep[Boolean] = unit(false)) = { // unroll at call site for parallelism (temporary until we have composite op)
      val scharrYkernel = Matrix[Int](3, 3)
      scharrYkernel(0,0) = -3; scharrYkernel(0,1) = -10; scharrYkernel(0,2) = -3
      scharrYkernel(2,0) =  3; scharrYkernel(2,1) =  10; scharrYkernel(2,2) =  3
      val scharrXkernel = scharrYkernel.t
      val a = x.convolve(scharrXkernel)
      val b = x.convolve(scharrYkernel)
      if (polar) cartToPolar(a.toFloat,b.toFloat) else (a.toFloat,b.toFloat)
    }
    // TODO: need to refactor using CanBuildFrom and 2.8 techniques to avoid this duplication.
    //def convolve(kernel: Rep[Matrix[Int]]) = GrayscaleImage(x.windowedFilter(kernel.numRows, kernel.numCols) { slice => (slice *:* kernel).sum })
    def windowedFilter(rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[Matrix[Int]] => Rep[Int]) =
      GrayscaleImage(image_windowed_filter(x,rowDim, colDim, block))
  }

  // object defs
  def grayscaleimage_obj_new(numRows: Rep[Int], numCols: Rep[Int]): Rep[GrayscaleImage]
  def grayscaleimage_obj_frommat(x: Rep[Matrix[Int]]): Rep[GrayscaleImage]
  def grayscaleimage_obj_carttopolar(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]]): (Rep[Matrix[Float]],Rep[Matrix[Float]])
}


trait GrayscaleImageOpsExp extends GrayscaleImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GrayscaleImageObjectNew(numRows: Exp[Int], numCols: Exp[Int]) extends Def[GrayscaleImage]
  case class GrayscaleImageObjectFromMat(x: Exp[Matrix[Int]]) extends Def[GrayscaleImage]


  ////////////////////////////////
  // implemented via delite ops

  case class GrayscaleImageObjectCartToPolarMagnitude(inA: Exp[Matrix[Float]], inB: Exp[Matrix[Float]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => Math.sqrt(a*a + b*b).asInstanceOfL[Float]
  }

  case class GrayscaleImageObjectCartToPolarPhase(inA: Exp[Matrix[Float]], inB: Exp[Matrix[Float]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => (Math.atan2(b, a)*180/Math.Pi).asInstanceOfL[Float]
  }

  ////////////////////
  // object interface

  def grayscaleimage_obj_new(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(GrayscaleImageObjectNew(numRows, numCols))
  def grayscaleimage_obj_frommat(x: Exp[Matrix[Int]]) = reflectEffect(GrayscaleImageObjectFromMat(x))
  def grayscaleimage_obj_carttopolar(x: Exp[Matrix[Float]], y: Exp[Matrix[Float]]) = {
    val mag = reflectPure(GrayscaleImageObjectCartToPolarMagnitude(x,y))
    val phase = reflectPure(GrayscaleImageObjectCartToPolarPhase(x,y)) map { a => if (a < 0f) a + 360f else a } 
    (mag,phase)
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GrayscaleImageObjectCartToPolarPhase(a,b) => reflectPure(new { override val original = Some(f,e) } with GrayscaleImageObjectCartToPolarPhase(f(a),f(b)))(mtype(manifest[A]))
    case Reflect(GrayscaleImageObjectFromMat(x), u, es) => reflectMirrored(Reflect(GrayscaleImageObjectFromMat(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}


trait ScalaGenGrayscaleImageOps extends ScalaGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new generated.scala.GrayscaleImageImpl(" + quote(numRows) + "," + quote(numCols) + ")")
    case GrayscaleImageObjectFromMat(m) => emitValDef(sym, "new generated.scala.GrayscaleImageImpl(" + quote(m) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenGrayscaleImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenGrayscaleImageOps extends CGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
