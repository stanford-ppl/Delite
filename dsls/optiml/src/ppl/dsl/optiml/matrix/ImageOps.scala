package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{VariablesExp, Variables, DSLOpsExp, CGenBase, CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

trait ImageOps extends Variables {
  this: OptiML =>

  implicit def repToImageOps[A:Manifest](x: Rep[Image[A]]) = new ImageOpsCls(x)
  implicit def varToImageOps[A:Manifest](x: Var[Image[A]]) = new ImageOpsCls(readVar(x))
  implicit def imageToInterface[A:Manifest](lhs: Rep[Image[A]]) = new MInterface[A](new ImageOpsCls[A](lhs))
  implicit def imageVarToInterface[A:Manifest](lhs: Var[Image[A]]) = new MInterface[A](new ImageOpsCls[A](readVar(lhs)))
  
  implicit def imageBuilder[A:Manifest] = new MatrixBuilder[A,Image[A]] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]) = {
      Image[A](numRows, numCols)
    }
    def toIntf(x: Rep[Image[A]]): Interface[Matrix[A]] = imageToInterface(x)
  }  

  object Image {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = image_obj_new(numRows, numCols)
    def apply[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = image_obj_frommat(x)
  }

  /**
   * By extending DenseMatOpsCls, we preserve the Image[A] return type. We could also choose to simply add methods onto DenseMatrix 
   * in a similar fashion to GrayscaleImageOps to ImageOps, but we would lose our image-ness whenever a bulk operator is used (just
   * like GrayscaleImage currently does). We are still exploring what the best consistent way of handling this is. 
   */
  class ImageOpsCls[A:Manifest](val elem: Rep[Image[A]]) extends MatOpsCls[A] {
    type M[X] = Image[X]
    type V[X] = DenseVector[X]
    type Self = Image[A]

    def mA: Manifest[A] = manifest[A]
    def mM[B:Manifest]: Manifest[M[B]] = manifest[Image[B]]    
    def wrap(x: Rep[Image[A]]): Interface[Matrix[A]] = imageToInterface(x)
    def toOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B] = repToImageOps[B](x)
    def toIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]] = imageToInterface[B](x)        
    def builder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,M[B]] = imageBuilder[B]            
    def mV[B:Manifest]: Manifest[V[B]] = manifest[DenseVector[B]]
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = denseVecToInterface[B](x)        
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]

    // image ops
    def downsample(rowFactor: Rep[Int], colFactor: Rep[Int])(block: Rep[DenseMatrix[A]] => Rep[A])(implicit ctx: SourceContext) = image_downsample(x,rowFactor, colFactor, block)
    def windowedFilter[B:Manifest:Arith](rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[DenseMatrix[A]] => Rep[B])(implicit ctx: SourceContext) = image_windowed_filter(x,rowDim, colDim, block)
    def convolve(kernel: Rep[DenseMatrix[A]])(implicit a: Arith[A], ctx: SourceContext) = { //unroll at call-site for parallelism (temporary until we have composite op) image_convolve(x)
      x.windowedFilter(kernel.numRows, kernel.numCols) { slice =>
        (slice *:* kernel).sum }
    }
      
    // required Matrix implementation -- just forward to DenseMatrix
    // TODO: how to remove this duplication with DenseMatrixOps?
    //       extending DenseMatOpsCls doesn't work with the type aliases.
          
    // delite collection
    def dcSize(implicit ctx: SourceContext): Rep[Int] = densematrix_size(x)
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] = densematrix_rawapply(x,n)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = densematrix_rawupdate(x,n,y)

    // accessors
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = densematrix_apply(x,i,j)
    def numRows(implicit ctx: SourceContext) = densematrix_numrows(x)
    def numCols(implicit ctx: SourceContext) = densematrix_numcols(x)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = densematrix_vview(x,start,stride,length,isRow)

    // data operations
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = densematrix_update(x,i,j,y)
    def insertRow(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densematrix_insertrow(x,pos,y)
    def insertAllRows(pos: Rep[Int], y: Rep[MA])(implicit ctx: SourceContext) = densematrix_insertallrows(x,pos,y)
    def insertCol(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densematrix_insertcol(x,pos,y)
    def insertAllCols(pos: Rep[Int], y: Rep[MA])(implicit ctx: SourceContext) = densematrix_insertallcols(x,pos,y)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removerows(x,pos,len)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removecols(x,pos,len)

    // not supported by interface right now
    def *(y: Rep[MA])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = Image(densematrix_multiply(x,y))
    def inv(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = Image(densematrix_inverse(x))    
    def mapRows[B:Manifest](f: Rep[VectorView[A]] => Rep[DenseVector[B]])(implicit ctx: SourceContext) = Image(densematrix_maprows(x,f))
    def reduceRows(f: (Rep[DenseVector[A]],Rep[VectorView[A]]) => Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]] = densematrix_reducerows(x,f)    
  }
  
  // object defs
  def image_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[Image[A]]
  def image_obj_frommat[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Image[A]]

  // class defs
  def image_downsample[A:Manifest](x: Rep[Image[A]], rowFactor: Rep[Int], colFactor: Rep[Int], block: Rep[DenseMatrix[A]] => Rep[A])(implicit ctx: SourceContext): Rep[Image[A]]
  def image_windowed_filter[A:Manifest,B:Manifest:Arith](x: Rep[Image[A]], rowDim: Rep[Int], colDim: Rep[Int], block: Rep[DenseMatrix[A]] => Rep[B])(implicit ctx: SourceContext): Rep[Image[B]]
}


trait ImageOpsExp extends ImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class ImageObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Image[A]] {
    val mA = manifest[A]
  }
  case class ImageObjectFromMat[A:Manifest](x: Exp[DenseMatrix[A]]) extends Def[Image[A]] {
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

  def image_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(ImageObjectNew[A](numRows, numCols))
  def image_obj_frommat[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectEffect(ImageObjectFromMat(x))

  ///////////////////
  // class interface

  def image_downsample[A:Manifest](x: Exp[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Exp[DenseMatrix[A]] => Exp[A])(implicit ctx: SourceContext) = {
    val y = (unit(0) :: x.numRows / rowFactor, unit(0) :: x.numCols / colFactor) { (row, col) =>
      block(x.slice(rowFactor * row, rowFactor * row + rowFactor, colFactor * col, colFactor * col + colFactor))
    }
    Image(y)
    //Image(ImageDownsample(x, rowFactor, colFactor, block))
  }
  def image_windowed_filter[A:Manifest,B:Manifest:Arith](x: Exp[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Exp[DenseMatrix[A]] => Exp[B])(implicit ctx: SourceContext) = {
    // Need to enforce odd values for sliceRows and sliceCols
    val rowOffset = (rowDim - unit(1)) / unit(2)
    val colOffset = (colDim - unit(1)) / unit(2)
    val y = (unit(0) :: x.numRows, unit(0) :: x.numCols) { (row,col) =>
      if ((row >= rowOffset) && (row < x.numRows - rowOffset) && (col >= colOffset) && (col < x.numCols - colOffset)) {
        block(x.slice(row - rowOffset, row + rowOffset + unit(1), col - colOffset, col + colOffset + unit(1)))
      } else {
        unit(0).AsInstanceOf[B]
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
    case m@ImageObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.Image[" + remap(m.mA) + "]") + "(" + quote(numRows) + "," + quote(numCols) + ")")
    case m@ImageObjectFromMat(x) => emitValDef(sym, "new " + remap("generated.scala.Image[" + remap(m.mA) + "]") + "(" + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: ImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenImageOps extends CGenBase {
  val IR: ImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
