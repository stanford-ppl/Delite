package ppl.dsl.optiml.matrix

import ppl.dsl.optiml.CudaGenDataStruct
import ppl.dsl.optiml.{Vector, Matrix, Image}
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.{VariablesExp, Variables, DSLOpsExp, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.Config
import ppl.dsl.optiml.{OptiML, OptiMLExp}

trait ImageOps extends Variables {
  this: OptiML =>

  object Image {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = image_obj_new(numRows, numCols)
    def apply[A:Manifest](x: Rep[Matrix[A]]) = image_obj_frommat(x)
  }

  implicit def repImageToImageOps[A:Manifest](x: Rep[Image[A]]) = new imageOpsCls(x)
  implicit def varToImageOps[A:Manifest](x: Var[Image[A]]) = new imageOpsCls(readVar(x))

  class imageOpsCls[A:Manifest](x: Rep[Image[A]]) {
    def downsample(rowFactor: Rep[Int], colFactor: Rep[Int])(block: Rep[Matrix[A]] => Rep[A]) = image_downsample(x,rowFactor, colFactor, block)
    def windowedFilter[B:Manifest:Arith](rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[Matrix[A]] => Rep[B]) = image_windowed_filter(x,rowDim, colDim, block)
    def convolve(kernel: Rep[Matrix[A]])(implicit a: Arith[A]) = { //unroll at call-site for parallelism (temporary until we have composite op) image_convolve(x)
      x.windowedFilter(kernel.numRows, kernel.numCols) { slice =>
        (slice *:* kernel).sum }
    }
  }

  // object defs
  def image_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]): Rep[Image[A]]
  def image_obj_frommat[A:Manifest](x: Rep[Matrix[A]]): Rep[Image[A]]

  // class defs
  def image_downsample[A:Manifest](x: Rep[Image[A]], rowFactor: Rep[Int], colFactor: Rep[Int], block: Rep[Matrix[A]] => Rep[A]): Rep[Image[A]]
  def image_windowed_filter[A:Manifest,B:Manifest:Arith](x: Rep[Image[A]], rowDim: Rep[Int], colDim: Rep[Int], block: Rep[Matrix[A]] => Rep[B]): Rep[Image[B]]
}


trait ImageOpsExp extends ImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class ImageObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Image[A]] {
    val mA = manifest[A]
  }
  case class ImageObjectFromMat[A:Manifest](x: Exp[Matrix[A]]) extends Def[Image[A]] {
    val mA = manifest[A]
  }

  ////////////////////////////////
  // implemented via delite ops

  // TODO: represent these explicitly, see IndexVector2Ops
//  case class ImageDownsample[A:Manifest](x: Exp[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Exp[Matrix[A]] => Exp[A])
//    extends DeliteOpMap[Int,Vector[A],Vector] {
//
//  }
//
//  case class ImageWindowedFilter[A:Manifest,B:Manifest:Arith](x: Exp[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[Matrix[A]] => Exp[B])
//    extends DeliteOpMap[Int,Vector[A],Vector] {
//
//  }


  ////////////////////
  // object interface

  def image_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(ImageObjectNew[A](numRows, numCols))
  def image_obj_frommat[A:Manifest](x: Exp[Matrix[A]]) = reflectEffect(ImageObjectFromMat(x))

  ///////////////////
  // class interface

  def image_downsample[A:Manifest](x: Exp[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Exp[Matrix[A]] => Exp[A]) = {
    val y = (0 :: x.numRows / rowFactor, 0 :: x.numCols / colFactor) { (row, col) =>
      block(x.slice(rowFactor * row, rowFactor * row + rowFactor, colFactor * col, colFactor * col + colFactor))
    }
    Image(y)
    //Image(ImageDownsample(x, rowFactor, colFactor, block))
  }
  def image_windowed_filter[A:Manifest,B:Manifest:Arith](x: Exp[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[Matrix[A]] => Exp[B]) = {
    // Need to enforce odd values for sliceRows and sliceCols
    val rowOffset = (rowDim - 1) / 2
    val colOffset = (colDim - 1) / 2
    val y = (0 :: x.numRows, 0 :: x.numCols) { (row,col) =>
      if ((row >= rowOffset) && (row < x.numRows - rowOffset) && (col >= colOffset) && (col < x.numCols - colOffset)) {
        block(x.slice(row - rowOffset, row + rowOffset + 1, col - colOffset, col + colOffset + 1))
      } else {
        unit(0).asInstanceOfL[B]
      }
    }
    Image(y)
    //Image(ImageWindowedFilter(x, rowDim, colDim, block))
  }
}


trait ScalaGenImageOps extends ScalaGenBase {
  val IR: ImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case m@ImageObjectNew(numRows, numCols) => emitValDef(sym, "new generated.scala.ImageImpl[" + remap(m.mA) + "](" + quote(numRows) + "," + quote(numCols) + ")")
    case m@ImageObjectFromMat(x) => emitValDef(sym, "new generated.scala.ImageImpl[" + remap(m.mA) + "](" + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: ImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenImageOps extends CGenBase {
  val IR: ImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
