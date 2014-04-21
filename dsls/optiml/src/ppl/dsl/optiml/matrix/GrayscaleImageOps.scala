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
  
  implicit def repToGrayscaleImageBuildableOps(x: Rep[GrayscaleImage]) = new GrayscaleImageBuildableOpsCls(x)
  implicit def varToGrayscaleImageBuildableOps(x: Var[GrayscaleImage]) = new GrayscaleImageBuildableOpsCls(readVar(x))    
  implicit def grayscaleImageToBuildableInterface(lhs: Rep[GrayscaleImage]) = new MBuildableInterface[Double](new GrayscaleImageBuildableOpsCls(lhs))
  implicit def grayscaleImageVarToBuildableInterface(lhs: Var[GrayscaleImage]) = new MBuildableInterface[Double](new GrayscaleImageBuildableOpsCls(readVar(lhs)))    
  implicit def repGrayscaleImageToGrayscaleImageOps(x: Rep[GrayscaleImage]) = new GrayscaleImageOpsCls(x)
  implicit def varToGrayscaleImageOps(x: Var[GrayscaleImage]) = new GrayscaleImageOpsCls(readVar(x))
  implicit def grayscaleImageToInterface(lhs: Rep[GrayscaleImage]) = new ImgInterface[Double](new GrayscaleImageOpsCls(lhs))
  implicit def grayscaleImageVarToInterface(lhs: Var[GrayscaleImage]) = new ImgInterface[Double](new GrayscaleImageOpsCls(readVar(lhs)))  
    
  implicit def grayscaleImageBuilder = new MatrixBuilder[Double,GrayscaleImage,GrayscaleImage] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]) = {
      GrayscaleImage(numRows, numCols)
    }
    def toBuildableIntf(x: Rep[GrayscaleImage]): Interface[MatrixBuildable[Double]] = grayscaleImageToBuildableInterface(x)
    def finalizer(x: Rep[GrayscaleImage]) = x.unsafeImmutable    
    def toIntf(x: Rep[GrayscaleImage]): Interface[Matrix[Double]] = grayscaleImageToInterface(x)
  }  
  
  object GrayscaleImage {
    def apply(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = grayscaleimage_obj_new(numRows, numCols)
    def apply(x: Rep[DenseMatrix[Double]])(implicit ctx: SourceContext) = grayscaleimage_obj_frommat(x)
    def cartToPolar(x: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]])(implicit ctx: SourceContext) = grayscaleimage_obj_carttopolar(x,y)
    // val scharrYkernel = Matrix(Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
    // val scharrXkernel = scharrYkernel.t
  }
  
  class GrayscaleImageBuildableOpsCls(val elem: Rep[GrayscaleImage]) extends ImageBuildableOpsCls[Double] {
    type Self = GrayscaleImage
    def mA = manifest[Double]    
    def wrap(x: Rep[GrayscaleImage]): Interface[MatrixBuildable[Double]] = grayscaleImageToBuildableInterface(x)    
  }
  
  class GrayscaleImageOpsCls(val elem: Rep[GrayscaleImage]) extends ImageOpsCls[Double] {
    import GrayscaleImage._
    
    type Self = GrayscaleImage
    type MA = GrayscaleImage
    def mMA = manifest[MA]
    def wrap(x: Rep[GrayscaleImage]): Interface[Image[Double]] = grayscaleImageToInterface(x)    
    def maToOps(x: Rep[MA]): MatOpsCls[Double] = repGrayscaleImageToGrayscaleImageOps(x)
    def maToIntf(x: Rep[MA]): Interface[Image[Double]] = grayscaleImageToInterface(x)    
    def maBuilder: MatrixBuilder[Double,MA,MA] = grayscaleImageBuilder    
    
    // delite collection
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = densematrix_rawapply(x,n)
    def dcUpdate(n: Rep[Int], y: Rep[Double])(implicit ctx: SourceContext): Rep[Unit] = densematrix_rawupdate(x,n,y)

    // accessors
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = densematrix_apply(x,i,j)
    def numRows(implicit ctx: SourceContext) = densematrix_numrows(x)
    def numCols(implicit ctx: SourceContext) = densematrix_numcols(x)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = densematrix_vview(x,start,stride,length,isRow)

    // not supported by interface right now
    def inv(implicit conv: Rep[Double] => Rep[Double], ctx: SourceContext) = densematrix_inverse(x)(manifest[Double],conv,ctx)
    
    // grayscale image
    def bitwiseOrDownsample(implicit ctx: SourceContext) = x.downsample(unit(2),unit(2)) { slice => 
        slice(unit(0),unit(0)).AsInstanceOf[Int] | slice(unit(1),unit(0)).AsInstanceOf[Int] | slice(unit(0),unit(1)).AsInstanceOf[Int] | slice(unit(1),unit(1)).AsInstanceOf[Int] }
                
    def gradients(polar: Rep[Boolean] = unit(false))(implicit ctx: SourceContext) = { // unroll at call site for parallelism (temporary until we have composite op)
      val scharrYkernel = DenseMatrix[Double](unit(3), unit(3))
      scharrYkernel(unit(0),unit(0)) = unit(-3); scharrYkernel(unit(0),unit(1)) = unit(-10); scharrYkernel(unit(0),unit(2)) = unit(-3)
      scharrYkernel(unit(2),unit(0)) = unit(3); scharrYkernel(unit(2),unit(1)) = unit(10); scharrYkernel(unit(2),unit(2)) = unit(3)
      val scharrXkernel = scharrYkernel.t
      val a = x.convolve(scharrXkernel)
      val b = x.convolve(scharrYkernel)
      if (polar) cartToPolar(a,b) else (a,b)
    }        
    def histogram = grayscaleimage_histogram(x)    
    
    // specialized version enables operating on GrayscaleImages directly instead of just Interface[Image[Double]]
    // however, now we have bit-rot; how can we prevent the redundancy with ImageOps?
    def windowedFilter(rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[GrayscaleImage] => Rep[Double])(implicit ctx: SourceContext) =
      grayscaleimage_windowed_filter(x,rowDim,colDim,block)
  }
  
  // object defs
  def grayscaleimage_obj_new(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_frommat(x: Rep[DenseMatrix[Double]])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_carttopolar(x: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]])(implicit ctx: SourceContext): (Rep[DenseMatrix[Double]],Rep[DenseMatrix[Double]])
  
  // class defs
  def grayscaleimage_histogram(x: Rep[GrayscaleImage]): Rep[DenseVector[Double]]
  def grayscaleimage_windowed_filter(x: Rep[GrayscaleImage], rowDim: Rep[Int], colDim: Rep[Int], block: Rep[GrayscaleImage] => Rep[Double]): Rep[GrayscaleImage]
}


trait GrayscaleImageOpsExp extends GrayscaleImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GrayscaleImageObjectNew(numRows: Exp[Int], numCols: Exp[Int]) extends Def[GrayscaleImage]

  ////////////////////////////////
  // implemented via delite ops

  case class GrayscaleImageObjectFromMat(in: Exp[DenseMatrix[Double]])
    extends DeliteOpMap[Double,Double,GrayscaleImage] {
      
    override def alloc = GrayscaleImage(in.numCols, in.numRows)
    val size = copyTransformedOrElse(_.size)(in.size)
    def func = i => i // parallel copy 
  }
  
  case class GrayscaleImageObjectCartToPolarMagnitude(intfA: Interface[Matrix[Double]], intfB: Interface[Matrix[Double]])
    extends MatrixArithmeticZipWith[Double,DenseMatrix[Double],DenseMatrix[Double]] {

    def func = (a,b) => sqrt(a*a + b*b)
  }

  case class GrayscaleImageObjectCartToPolarPhase(intfA: Interface[Matrix[Double]], intfB: Interface[Matrix[Double]])
    extends MatrixArithmeticZipWith[Double,DenseMatrix[Double],DenseMatrix[Double]] {
      
    def func = (a,b) => (atan2(b, a)*unit(180)/Pi)
  }

  case class GrayscaleImageHistogram(x: Exp[GrayscaleImage]) extends DeliteOpSingleTask[DenseVector[Double]](reifyEffectsHere(grayscaleimage_histogram_impl(x)))
  
  case class GrayscaleImageWindowedFilter(x: Exp[GrayscaleImage], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[GrayscaleImage] => Exp[Double], out: Exp[GrayscaleImage])
    extends DeliteOpForeach[Int] {
    
    val rowOffset = (rowDim - unit(1)) / unit(2)
    val colOffset = (colDim - unit(1)) / unit(2)

    val in = copyTransformedOrElse(_.in)(unit(0) :: x.numRows)
    val size = copyTransformedOrElse(_.size)(x.numRows)
    def sync = i => List[Int]()
    def func = row => out(row) = (unit(0) :: x.numCols) map { col => 
      if ((row >= rowOffset) && (row < x.numRows - rowOffset) && (col >= colOffset) && (col < x.numCols - colOffset)) {
        // workaround for: https://github.com/stanford-ppl/Delite/issues/26
        val z = var_new(x.slice(row - rowOffset, row + rowOffset + unit(1), col - colOffset, col + colOffset + unit(1)))
        block(readVar(z))
      } else {
        unit(0.0)
      }
    }
  }
  
  
  ////////////////////
  // object interface

  def grayscaleimage_obj_new(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(GrayscaleImageObjectNew(numRows, numCols))
  def grayscaleimage_obj_frommat(x: Exp[DenseMatrix[Double]])(implicit ctx: SourceContext) = reflectEffect(GrayscaleImageObjectFromMat(x))
  def grayscaleimage_obj_carttopolar(x: Exp[DenseMatrix[Double]], y: Exp[DenseMatrix[Double]])(implicit ctx: SourceContext) = {
    val mag = reflectPure(GrayscaleImageObjectCartToPolarMagnitude(x,y))
    val phase = reflectPure(GrayscaleImageObjectCartToPolarPhase(x,y)) map { a => if (a < unit(0.)) a + unit(360.) else a } 
    (mag,phase)
  }
  
  ///////////////////
  // class interface
  
  def grayscaleimage_histogram(x: Exp[GrayscaleImage]) = reflectPure(GrayscaleImageHistogram(x))
  def grayscaleimage_windowed_filter(x: Exp[GrayscaleImage], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[GrayscaleImage] => Exp[Double]) = {
    val out = GrayscaleImage(x.numRows, x.numCols)
    reflectWrite(out)(GrayscaleImageWindowedFilter(x,rowDim,colDim,block,out))
    out.unsafeImmutable                
  }    

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GrayscaleImageObjectCartToPolarPhase(a,b) => reflectPure(new { override val original = Some(f,e) } with GrayscaleImageObjectCartToPolarPhase(f.intf(a),f.intf(b)))(mtype(manifest[A]), implicitly[SourceContext])
    case Reflect(GrayscaleImageObjectFromMat(x), u, es) => reflectMirrored(Reflect(GrayscaleImageObjectFromMat(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}


trait ScalaGenGrayscaleImageOps extends ScalaGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.GrayscaleImage")+"(" + quote(numRows) + "," + quote(numCols) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenGrayscaleImageOps extends CudaGenBase {
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
