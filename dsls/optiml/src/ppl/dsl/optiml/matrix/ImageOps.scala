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

  /**
   * Interface Image[A]
   */
  class ImgInterface[A:Manifest](override val ops: ImageOpsCls[A]) extends MInterface[A](ops) with Interface[Image[A]]  
  implicit def interfaceToImageOps[A:Manifest](intf: Interface[Image[A]]): InterfaceImageOpsCls[A] = new InterfaceImageOpsCls[A](intf.asInstanceOf[ImgInterface[A]])  
  
  abstract class ImageBuildableOpsCls[A:Manifest] extends MatBuildableOpsCls[A] {
    type M[X] = DenseMatrix[X]
    type V[X] = DenseVector[X]
    type Self <: DenseMatrix[A]
    def toIntf[B:Manifest](x: Rep[M[B]]) = denseMatToBuildableInterface(x)
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = denseVecToInterface[B](x)            
    
    // FIXME: see MatrixBuildableOps.scala
    protected def _numRows(implicit ctx: SourceContext) = densematrix_numrows(x)
    protected def _numCols(implicit ctx: SourceContext) = densematrix_numcols(x)
                
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = densematrix_update(x,i,j,y)
    def insertRow(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = densematrix_insertrow(x,pos,y)
    def insertAllRows(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = densematrix_insertallrows(x,pos,y)
    def insertCol(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = densematrix_insertcol(x,pos,y)
    def insertAllCols(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = densematrix_insertallcols(x,pos,y)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removerows(x,pos,len)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removecols(x,pos,len)
  }
  
  abstract class ImageOpsCls[A:Manifest] extends MatOpsCls[A] with InterfaceOps[Image[A]] {
    type V[X] = DenseVector[X]
    type M[X] = DenseMatrix[X]    
    type I[X] = DenseMatrix[X]
    type View[X] = DenseVectorView[X]      
    type MA <: Image[A]
    type IA = MA
    implicit def wrap(x: Rep[Self]): Interface[Image[A]]
    implicit def maBuilder: MatrixBuilder[A,MA,MA]    
    implicit def maToIntf(x: Rep[MA]): Interface[Image[A]]
    def mA: Manifest[A] = manifest[A]
    def mIA = mMA
    def mV[B:Manifest]: Manifest[V[B]] = manifest[DenseVector[B]]
    def mM[B:Manifest]: Manifest[M[B]] = manifest[DenseMatrix[B]]
    def mI[B:Manifest]: Manifest[I[B]] = mM[B]
    def toIntf[B:Manifest](x: Rep[M[B]]) = denseMatToBuildableInterface(x)        
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = denseVecToInterface[B](x)        
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]
    def viewToIntf[B:Manifest](x: Rep[View[B]]) = denseViewToInterface(x)
    def matToOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B] = repToDenseMatOps[B](x)
    def matToIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]] = denseMatToInterface[B](x)        
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = denseMatrixBuilder[B]                    

    // image ops
    def downsample(rowFactor: Rep[Int], colFactor: Rep[Int])(block: Interface[Image[A]] => Rep[A])(implicit ctx: SourceContext) = image_downsample[A,MA](x, rowFactor, colFactor, block) //(manifest[A], manifest[Image[A]], imageBuilder[A], implicitly[SourceContext])
    def windowedFilter[B:Manifest:Arith](rowDim: Rep[Int], colDim: Rep[Int])(block: Interface[Image[A]] => Rep[B])(implicit ctx: SourceContext) = image_windowed_filter[A,B,M[B]](x,rowDim, colDim, block) //(manifest[A], manifest[Image[A]], manifest[B], implicitly[Arith[B]], manifest[Image[B]], imageBuilder[B], implicitly[SourceContext])
    def convolve(kernel: Rep[DenseMatrix[A]])(implicit a: Arith[A], ctx: SourceContext) = { //unroll at call-site for parallelism (temporary until we have composite op) image_convolve(x)
      windowedFilter(kernel.numRows, kernel.numCols) { slice =>
        (slice *:* kernel).sum }
    }      
  }
    
  class InterfaceImageOpsCls[A:Manifest](override val intf: ImgInterface[A]) extends InterfaceMatOpsCls[A](intf) {
    // ugh.. operations on Interface[Image] return an Interface[Matrix] unless we override them here to call the proper maToIntf method. this sucks.
    // we should have the same problems with operations on Interface[IndexVector] returning an Interface[Vector]... multi-level interfaces don't quite work the way we'd like.
    override def slice(startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int])(implicit ctx: SourceContext) = intf.ops.maToIntf(intf.ops.slice(startRow,endRow,startCol,endCol))
    
    def downsample(rowFactor: Rep[Int], colFactor: Rep[Int])(block: Interface[Image[A]] => Rep[A])(implicit ctx: SourceContext) = intf.ops.maToIntf(intf.ops.downsample(rowFactor,colFactor)(block))
    def windowedFilter[B:Manifest:Arith](rowDim: Rep[Int], colDim: Rep[Int])(block: Interface[Image[A]] => Rep[B])(implicit ctx: SourceContext) = intf.ops.matToIntf(intf.ops.windowedFilter(rowDim,colDim)(block))
    def convolve(kernel: Rep[DenseMatrix[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.matToIntf(intf.ops.convolve(kernel))
  }
  
  // class defs
  def image_downsample[A:Manifest,IA<:Image[A]:Manifest](x: Interface[Image[A]], rowFactor: Rep[Int], colFactor: Rep[Int], block: Interface[Image[A]] => Rep[A])(implicit b: MatrixBuilder[A,IA,IA], ctx: SourceContext): Rep[IA]
  def image_windowed_filter[A:Manifest,B:Manifest:Arith,MB<:Matrix[B]:Manifest](x: Interface[Image[A]], rowDim: Rep[Int], colDim: Rep[Int], block: Interface[Image[A]] => Rep[B])(implicit b: MatrixBuilder[B,MB,MB], ctx: SourceContext): Rep[MB]
}


trait ImageOpsExp extends ImageOps with VariablesExp {
  this: OptiMLExp  =>
    
  case class ImageDownsample[A:Manifest](x: Interface[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Interface[Image[A]] => Exp[A], out: Interface[MatrixBuildable[A]])
    extends DeliteOpForeach[Int] {

    val in = copyTransformedOrElse(_.in)(unit(0) :: x.numRows / rowFactor)
    val size = copyTransformedOrElse(_.size)(x.numRows / rowFactor)
    def sync = i => List[Int]()
    def func = row => out(row) = (unit(0) :: x.numCols / colFactor) map { col => 
      block(x.slice(rowFactor * row, rowFactor * row + rowFactor, colFactor * col, colFactor * col + colFactor)) 
    } 
  }
 

  case class ImageWindowedFilter[A:Manifest,B:Manifest:Arith](x: Interface[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Interface[Image[A]] => Exp[B], out: Interface[MatrixBuildable[B]])
    extends DeliteOpForeach[Int] {
    
    val rowOffset = (rowDim - unit(1)) / unit(2)
    val colOffset = (colDim - unit(1)) / unit(2)

    val in = copyTransformedOrElse(_.in)(unit(0) :: x.numRows)
    val size = copyTransformedOrElse(_.size)(x.numRows)
    def sync = i => List[Int]()
    def func = row => out(row) = (unit(0) :: x.numCols) map { col => 
      if ((row >= rowOffset) && (row < x.numRows - rowOffset) && (col >= colOffset) && (col < x.numCols - colOffset)) {
        block(x.slice(row - rowOffset, row + rowOffset + unit(1), col - colOffset, col + colOffset + unit(1)))
      } else {
        unit(0).AsInstanceOf[B]
      }
    }
  }


  ///////////////////
  // class interface

  def image_downsample[A:Manifest,IA<:Image[A]:Manifest](x: Interface[Image[A]], rowFactor: Exp[Int], colFactor: Exp[Int], block: Interface[Image[A]] => Exp[A])(implicit b: MatrixBuilder[A,IA,IA], ctx: SourceContext) = {
    val out = b.alloc(x.numRows / rowFactor, x.numCols / colFactor)
    reflectWrite(out)(ImageDownsample(x,rowFactor,colFactor,block,b.toBuildableIntf(out)))
    out.unsafeImmutable            
  }
  def image_windowed_filter[A:Manifest,B:Manifest:Arith,MB<:Matrix[B]:Manifest](x: Interface[Image[A]], rowDim: Exp[Int], colDim: Exp[Int], block: Interface[Image[A]] => Exp[B])(implicit b: MatrixBuilder[B,MB,MB], ctx: SourceContext) = {
    val out = b.alloc(x.numRows, x.numCols)
    reflectWrite(out)(ImageWindowedFilter(x,rowDim,colDim,block,b.toBuildableIntf(out)))
    out.unsafeImmutable                
  }
}


trait ScalaGenImageOps extends ScalaGenBase {
  val IR: ImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case m@ImageObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.Image[" + remap(m.mA) + "]") + "(" + quote(numRows) + "," + quote(numCols) + ")")
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CudaGenImageOps extends CudaGenBase {
  val IR: ImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenImageOps extends CGenBase {
  val IR: ImageOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
