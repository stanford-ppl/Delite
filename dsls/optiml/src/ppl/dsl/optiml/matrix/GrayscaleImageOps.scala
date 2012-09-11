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

  implicit def grayscaleImageBuilder = new MatrixBuilder[Int,GrayscaleImage,GrayscaleImage] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]) = {
      GrayscaleImage(numRows, numCols)
    }
    def toBuildableIntf(x: Rep[GrayscaleImage]): Interface[MatrixBuildable[Int]] = imageToBuildableInterface(x)
    def finalizer(x: Rep[GrayscaleImage]) = x.unsafeImmutable    
    def toIntf(x: Rep[GrayscaleImage]): Interface[Matrix[Int]] = imageToInterface(x)
  }  
  
  object GrayscaleImage {
    def apply(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = grayscaleimage_obj_new(numRows, numCols)
    def apply(x: Rep[DenseMatrix[Int]])(implicit ctx: SourceContext) = grayscaleimage_obj_frommat(x)
    def cartToPolar(x: Rep[DenseMatrix[Float]], y: Rep[DenseMatrix[Float]])(implicit ctx: SourceContext) = grayscaleimage_obj_carttopolar(x,y)

//    val scharrYkernel = Matrix(Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
//    val scharrXkernel = scharrYkernel.t
  }

  implicit def repGrayscaleImageToGrayscaleImageOps(x: Rep[GrayscaleImage]) = new grayscaleImageOpsCls(x)
  implicit def varToGrayscaleImageOps(x: Var[GrayscaleImage]) = new grayscaleImageOpsCls(readVar(x))

  class grayscaleImageOpsCls(x: Rep[GrayscaleImage]) {
    import GrayscaleImage._

    def bitwiseOrDownsample(implicit ctx: SourceContext) = GrayscaleImage(x.downsample(unit(2),unit(2)) { slice => slice(unit(0),unit(0)) | slice(unit(1),unit(0)) | slice(unit(0),unit(1)) | slice(unit(1),unit(1)) })
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
    def histogram = grayscaleimage_histogram(x)
    
    // TODO: we want block to be Rep[GrayscaleImage] => Rep[Int], but block operates on a slice, and the slice of a GrayscaleImage is currently an Image.
    // GrayscaleImage should stop cheating and implement the full Matrix (or Image?) interface itself, and then we should be able to do this cleanly
    // (would also need to pass the ops along to get the slice dispatch correct)
    def downsample(rowFactor: Rep[Int], colFactor: Rep[Int])(block: Rep[Image[Int]] => Rep[Int])(implicit ctx: SourceContext) = image_downsample[Int,GrayscaleImage](x, rowFactor, colFactor, block)
    def windowedFilter(rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[Image[Int]] => Rep[Int])(implicit ctx: SourceContext) = image_windowed_filter[Int,GrayscaleImage,Int,GrayscaleImage](x,rowDim, colDim, block)    
    // def windowedFilter(rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[GrayscaleImage] => Rep[Int])(implicit ctx: SourceContext) =
    //   GrayscaleImage(image_windowed_filter(x,rowDim, colDim, block))
  }

  // object defs
  def grayscaleimage_obj_new(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_frommat(x: Rep[DenseMatrix[Int]])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_carttopolar(x: Rep[DenseMatrix[Float]], y: Rep[DenseMatrix[Float]])(implicit ctx: SourceContext): (Rep[DenseMatrix[Float]],Rep[DenseMatrix[Float]])
  
  // class defs
  def grayscaleimage_histogram(x: Rep[GrayscaleImage]): Rep[DenseVector[Int]]
}


trait GrayscaleImageOpsExp extends GrayscaleImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GrayscaleImageObjectNew(numRows: Exp[Int], numCols: Exp[Int]) extends Def[GrayscaleImage]

  ////////////////////////////////
  // implemented via delite ops

  case class GrayscaleImageObjectFromMat(in: Exp[DenseMatrix[Int]])
    extends DeliteOpMap[Int,Int,GrayscaleImage] {
      
    override def alloc = GrayscaleImage(in.numCols, in.numRows)
    val size = copyTransformedOrElse(_.size)(in.size)
    def func = i => i // parallel copy 
  }
  
  case class GrayscaleImageObjectCartToPolarMagnitude(intfA: Interface[Matrix[Float]], intfB: Interface[Matrix[Float]])
    extends MatrixArithmeticZipWith[Float,DenseMatrix[Float],DenseMatrix[Float]] {

    def func = (a,b) => sqrt(a*a + b*b).AsInstanceOf[Float]
  }

  case class GrayscaleImageObjectCartToPolarPhase(intfA: Interface[Matrix[Float]], intfB: Interface[Matrix[Float]])
    extends MatrixArithmeticZipWith[Float,DenseMatrix[Float],DenseMatrix[Float]] {
      
    def func = (a,b) => (atan2(b, a)*unit(180)/Pi).AsInstanceOf[Float]
  }

  case class GrayscaleImageHistogram(x: Rep[GrayscaleImage]) extends DeliteOpSingleTask[DenseVector[Int]](reifyEffectsHere(grayscaleimage_histogram_impl(x)))
  
  ////////////////////
  // object interface

  def grayscaleimage_obj_new(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(GrayscaleImageObjectNew(numRows, numCols))
  def grayscaleimage_obj_frommat(x: Exp[DenseMatrix[Int]])(implicit ctx: SourceContext) = reflectEffect(GrayscaleImageObjectFromMat(x))
  def grayscaleimage_obj_carttopolar(x: Exp[DenseMatrix[Float]], y: Exp[DenseMatrix[Float]])(implicit ctx: SourceContext) = {
    val mag = reflectPure(GrayscaleImageObjectCartToPolarMagnitude(x,y))
    val phase = reflectPure(GrayscaleImageObjectCartToPolarPhase(x,y)) map { a => if (a < unit(0f)) a + unit(360f) else a } 
    (mag,phase)
  }
  
  ///////////////////
  // class interface
  
  def grayscaleimage_histogram(x: Exp[GrayscaleImage]) = reflectPure(GrayscaleImageHistogram(x))
    

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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.Image[Int]")+"(" + quote(numRows) + "," + quote(numCols) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenGrayscaleImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: GrayscaleImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenGrayscaleImageOps extends CGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
