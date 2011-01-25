package ppl.dsl.optiml

import datastruct.CudaGenDataStruct
import datastruct.scala.{MatrixImpl, VectorImpl, Vector, Matrix, GrayscaleImage}
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.delite.framework.Config

trait GrayscaleImageOps extends DSLType with Variables {
  this: OptiML =>

  object GrayscaleImage {
    def apply(numRows: Rep[Int], numCols: Rep[Int]) = grayscaleimage_obj_new(numRows, numCols)
    def cartToPolar(x: Rep[GrayscaleImage], y: Rep[GrayscaleImage]) = grayscaleimage_obj_carttopolar(x,y)
  }

  implicit def repGrayscaleImageToGrayscaleImageOps[A:Manifest](x: Rep[GrayscaleImage]) = new grayscaleImageRepCls(x)
  implicit def varToGrayscaleImageOps[A:Manifest](x: Var[GrayscaleImage]) = new grayscaleImageRepCls(readVar(x))

  class grayscaleImageRepCls(x: Rep[GrayscaleImage]) {
    def bitwiseOrDownsample() = x.downsample(2,2) { slice => slice(0,0) | slice(1,0) | slice(0,1) | slice(1,1) }
    def gradients(polar: Rep[Boolean] = false) = { // unroll at call site for parallelism (temporary until we have composite op)
      val x = convolve(scharrXkernel)
      val y = convolve(scharrYkernel)
      if (polar) cartToPolar(x,y) else (x,y)
    }
  }

  // object defs
  def grayscaleimage_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]): Rep[GrayscaleImage[A]]
  def grayscaleimage_obj_carttopolar(x: Rep[GrayscaleImage], y: Rep[GrayscaleImage]): (Rep[Matrix[Float]],Rep[Matrix[Float]])

  // class defs

}


trait GrayscaleImageOpsExp extends GrayscaleImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GrayscaleImageObjectNew(numRows: Exp[Int], numCols: Exp[Int]) extends Def[GrayscaleImage] {
     val mI = manifest[GrayscaleImageImpl]
  }

  ////////////////////////////////
  // implemented via delite ops

  case class GrayscaleImageObjectCartToPolarMagnitude(inA: Exp[GrayscaleImage], inB: Exp[GrayscaleImage])
    extends DeliteOpZipWith[Int,Int,Float,Matrix] {

    val alloc = reifyEffects(Matrix[Float](inA.numRows, inA.numCols))
    val v = (fresh[Int],fresh[Int])
    val func = Math.sqrt(v._1*v._1 + v._2*v._2).asInstanceOfL[Float]
  }

  case class GrayscaleImageObjectCartToPolarPhase(inA: Exp[GrayscaleImage], inB: Exp[GrayscaleImage])
    extends DeliteOpZipWith[Int,Int,Float,Matrix] {

    val alloc = reifyEffects(Matrix[Float](inA.numRows, inA.numCols))
    val v = (fresh[Int],fresh[Int])
    val func = (Math.atan2(b, a)*180/Math.Pi).asInstanceOf[FloatL]
  }

  ////////////////////
  // object interface

  def grayscaleimage_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(GrayscaleImageObjectNew[A](numRows, numCols))
  def grayscaleimage_obj_carttopolar(x: Exp[GrayscaleImage], y: Exp[GrayscaleImage]) = {
    val mag = GrayscaleImageCartToPolarMagnitude(x,y)
    val phase = GrayscaleImageCartToPolarPhase(x,y) mmap { a => if (a < 0) a + 360 else a }
    (mag,phase)
  }

  ///////////////////
  // class interface

  def grayscaleimage_downsample[A:Manifest](x: Exp[GrayscaleImage[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Exp[Matrix[A]] => Exp[A]) = GrayscaleImageDownsample(x, rowFactor, colFactor, block)
  def grayscaleimage_windowed_filter[A:Manifest,B:Manifest:Arith](x: Exp[GrayscaleImage[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[Matrix[A]] => Exp[B]) = GrayscaleImageWindowedFilter(x, rowDim, colDim, block)
}


trait ScalaGenGrayscaleImageOps extends ScalaGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  case m@GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap(m.mI) + "(" + quote(numRows) + "," + quote(numCols) + ")")
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenGrayscaleImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {  
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenGrayscaleImageOps extends CGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {  
    case _ => super.emitNode(sym, rhs)
  }
}