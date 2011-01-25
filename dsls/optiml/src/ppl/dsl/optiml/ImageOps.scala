package ppl.dsl.optiml

import datastruct.CudaGenDataStruct
import datastruct.scala.{MatrixImpl, VectorImpl, Vector, Matrix, Image}
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.delite.framework.Config

trait ImageOps extends DSLType with Variables {
  this: OptiML =>

  object Image {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = image_obj_new(numRows, numCols)
  }

  implicit def repImageToImageOps[A:Manifest](x: Rep[Image[A]]) = new imageRepCls(x)
  implicit def varToImageOps[A:Manifest](x: Var[Image[A]]) = new imageRepCls(readVar(x))

  class imageRepCls[A:Manifest](x: Rep[Image[A]]) {
    def downsample(rowFactor: Rep[Int], colFactor: Rep[Int])(block: Rep[Matrix[A]] => Rep[A]) = image_downsample(x,rowFactor, colFactor, block)
    def windowedFilter[B:Manifest:Arith](rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[Matrix[A]] => Rep[B]) = image_windowed_filter(x,rowDim, colDim, block)
    def convolve(kernel: Matrix[A])(implicit a: Arith[A]) = { //unroll at call-site for parallelism (temporary until we have composite op) image_convolve(x)
      x.windowedFilter(kernel.numRows, kernel.numCols) { slice => (slice *:* kernel).sum }
    }
  }

  // object defs
  def image_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]): Rep[Image[A]]

  // class defs
  def image_downsample[A:Manifest](x: Rep[Image[A]], rowFactor: Rep[Int], colFactor: Rep[Int], block: Rep[Matrix[A]] => Rep[A]): Rep[Image[A]]
  def image_windowed_filter[A:Manifest,B:Manifest:Arith](x: Rep[Image[A]], rowDim: Rep[Int], colDim: Rep[Int], block: Rep[Matrix[A]] => Rep[B]): Rep[Image[B]]
}


trait ImageOpsExp extends ImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class ImageObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Image[A]] {
     val mI = manifest[ImageImpl[A]]
  }

  ////////////////////////////////
  // implemented via delite ops

  case class ImageDownsample[A:Manifest](x: Exp[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Exp[Matrix[A]] => Exp[A])
    extends DeliteOpMap[A,A,Image] {

    val inA = (0::numRows/rowFactor, 0::numCols/colFactor)
    val alloc = reifyEffects(Image[A](inA.numRows, inA.numCols))
    val v = (fresh[A],fresh[A])
    val func = reifyEffects(block(x.slice(rowFactor*v._1, rowFactor*v._1 + rowFactor, colFactor*v._2, colFactor*v._2 + colFactor )))
  }

  case class ImageWindowedFilter[A:Manifest,B:Manifest:Arith](x: Exp[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[Matrix[A]] => Exp[B])
    extends DeliteOpMap[A,B,Image] {

    val inA = (0::numRows,0::numCols)
    val alloc = reifyEffects(Image[B](inA.numRows, inA.numCols))
    val v = (fresh[A],fresh[A])

    // need to check that rowDim and colDim are odd
    val rowOffset = (rowDim - 1) / 2
    val colOffset = (colDim - 1) / 2
    val func = reifyEffects {
      if ((v._1 >= rowOffset) && (v._1 < numRows - rowOffset) && (v._2 >= colOffset) && (v._2 < numCols - colOffset)) {
        block(x.slice(v._1 - rowOffset, v._1 + rowOffset + 1, v._2 - colOffset, v._2 + colOffset + 1))
      } else {
        unit(0).asInstanceOfL[B]
      }
    }
  }


  ////////////////////
  // object interface

  def image_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(ImageObjectNew[A](numRows, numCols))

  ///////////////////
  // class interface

  def image_downsample[A:Manifest](x: Exp[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Exp[Matrix[A]] => Exp[A]) = ImageDownsample(x, rowFactor, colFactor, block)
  def image_windowed_filter[A:Manifest,B:Manifest:Arith](x: Exp[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[Matrix[A]] => Exp[B]) = ImageWindowedFilter(x, rowDim, colDim, block)
}


trait ScalaGenImageOps extends ScalaGenBase {
  val IR: ImageOpsExp
  import IR._

  case m@ImageObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap(m.mI) + "(" + quote(numRows) + "," + quote(numCols) + ")")
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: ImageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {  
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenImageOps extends CGenBase {
  val IR: ImageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {  
    case _ => super.emitNode(sym, rhs)
  }
}