package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{DSLOpsExp, FunctionBlocksExp}
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenEffect, ScalaGenEffect, CudaGenBase, ScalaGenBase, CGenBase, OpenCLGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._
import ppl.dsl.optila.SparseTransform
import ppl.dsl.optiml._

trait OptiMLDenseMatrixOps extends ppl.dsl.optila.matrix.DenseMatrixOps {
  this: OptiML =>
  
  implicit def denseToMatOverrides[A:Manifest](x: Rep[DenseMatrix[A]]) = new OptiMLDenseMatOpsOverrides(x)  
}

trait OptiMLSparseMatrixOps extends ppl.dsl.optila.matrix.SparseMatrixOps {
  this: OptiML =>
  
  implicit def sparseToMatOverrides[A:Manifest](x: Rep[SparseMatrix[A]]) = new OptiMLSparseMatOpsOverrides(x)  
}

trait GrayscaleImageOpsExtension extends GrayscaleImageOps {
  this: OptiML =>
  
  implicit def grayscaleImageToMatOverrides(x: Rep[GrayscaleImage]) = new OptiMLGrayscaleImageOpsOverrides(x)  
}

trait MatrixOps extends ppl.dsl.optila.matrix.MatrixOps  {
  this: OptiML =>

  trait OptiMLMatOpsOverrides[A] extends MatOpsCls[A] {
    def apply(rowIndices: Interface[IndexVector])(implicit ctx: SourceContext) = matrix_apply_row_indices[A,IA,MA](x, rowIndices)
    def apply(rowIndices: Interface[IndexVector], colIndices: IndexWildcard)(implicit ctx: SourceContext) = matrix_apply_row_indices[A,IA,MA](x, rowIndices)
    def apply(rowIndices: IndexWildcard, colIndices: Interface[IndexVector])(implicit ctx: SourceContext) = matrix_apply_col_indices[A,IA,MA](x, colIndices)
    def apply(rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit ctx: SourceContext) = matrix_apply_block_indices[A,IA,MA](x, rowIndices, colIndices)    
  }
  
  class OptiMLDenseMatOpsOverrides[A:Manifest](x: Rep[DenseMatrix[A]]) extends DenseMatOpsCls(x) with OptiMLMatOpsOverrides[A] 
  class OptiMLSparseMatOpsOverrides[A:Manifest](x: Rep[SparseMatrix[A]]) extends SparseMatOpsCls(x) with OptiMLMatOpsOverrides[A] {
    // the SourceContext implicit is ambiguous with the possible following apply (Vector constructor) call
    //def nzRowIndices(implicit ctx: SourceContext) = sparsematrix_nz_row_indices(x)
    def nzRowIndices = sparsematrix_nz_row_indices(x)
  }
  class OptiMLGrayscaleImageOpsOverrides(x: Rep[GrayscaleImage]) extends GrayscaleImageOpsCls(x) with OptiMLMatOpsOverrides[Double] 

  // class defs
  def matrix_apply_row_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA] 
  def matrix_apply_col_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA]   
  def matrix_apply_block_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA]  
  
  def sparsematrix_nz_row_indices[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[IndexVectorDense]
}

trait MatrixOpsExp extends ppl.dsl.optila.matrix.MatrixOpsExp with MatrixOps with VariablesExp with SparseTransform {
  this: OptiMLExp  =>
 
  ////////////////////////////////
  // implemented via delite ops

  case class MatrixApplyRowIndices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit val b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleWithManifest2[A,I,MA](reifyEffectsHere(matrix_apply_row_indices_impl[A,I,MA](x,rowIndices)))       
  
  case class MatrixApplyColIndices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit val b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleWithManifest2[A,I,MA](reifyEffectsHere(matrix_apply_col_indices_impl[A,I,MA](x,colIndices)))       

  case class MatrixApplyBlockIndices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit val b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleWithManifest2[A,I,MA](reifyEffectsHere(matrix_apply_block_indices_impl[A,I,MA](x,rowIndices,colIndices)))       
    
  case class SparseMatrixNZRowIndices[A:Manifest](x: Rep[SparseMatrix[A]]) extends DeliteOpSingleWithManifest[A,IndexVectorDense](reifyEffectsHere(sparsematrix_csr_nz_row_indices_impl(x)))
  
  /////////////////////
  // class interface

  def matrix_apply_row_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext)
    = reflectPure(MatrixApplyRowIndices[A,I,MA](x,rowIndices))
  def matrix_apply_col_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext)
    = reflectPure(MatrixApplyColIndices[A,I,MA](x,colIndices))
  def matrix_apply_block_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext)
    = reflectPure(MatrixApplyBlockIndices[A,I,MA](x,rowIndices,colIndices))

  def sparsematrix_nz_row_indices[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixNZRowIndices(x))
  
  def sparsematrix_maprowstovecnz[A:Manifest,B:Manifest](x: Exp[SparseMatrix[A]], f: Exp[Int] => Exp[B], isRow: Exp[Boolean]) = {
    val indices = x.nzRowIndices
    val outIndices = densevector_raw_data(indices).take(indices.length) // trim
    val outData = outIndices.map(f)     
    val out = SparseVector[B](x.numRows, isRow)
    sparsevector_set_raw_indices(out, outIndices.unsafeImmutable)
    sparsevector_set_raw_data(out, outData)
    sparsevector_set_nnz(out, outData.length)
    out.unsafeImmutable          
  }
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@MatrixApplyRowIndices(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixApplyRowIndices(f.intf(x),f.intf(y))(e.mA,e.mB,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])  
    case e@MatrixApplyColIndices(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixApplyColIndices(f.intf(x),f.intf(y))(e.mA,e.mB,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@MatrixApplyBlockIndices(x,r,c) => reflectPure(new { override val original = Some(f,e) } with MatrixApplyBlockIndices(f.intf(x),f.intf(r),f.intf(c))(e.mA,e.mB,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])        
    case e@SparseMatrixNZRowIndices(x) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixNZRowIndices(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])        
    case Reflect(e@MatrixApplyRowIndices(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixApplyRowIndices(f.intf(x),f.intf(y))(e.mA,e.mB,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)    
    case Reflect(e@MatrixApplyColIndices(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixApplyColIndices(f.intf(x),f.intf(y))(e.mA,e.mB,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@MatrixApplyBlockIndices(x,r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixApplyBlockIndices(f.intf(x),f.intf(r),f.intf(c))(e.mA,e.mB,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@SparseMatrixNZRowIndices(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixNZRowIndices(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx) 
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

  //////////////
  // transforms
  
  def specializeSparseMapRows[A:Manifest,B:Manifest](s: Sym[Any], e: MatrixMapRowsToVec[A,B,_], x: Exp[SparseMatrix[A]], rowFunc: Interface[Vector[A]] => Exp[B], isRow: Exp[Boolean]): Option[Exp[Any]] = {
    val default = ZeroVector[A](x.numRows, true) 
    val test = reifyEffects(rowFunc(default))
    val repr = e.body.asInstanceOf[DeliteCollectElem[B,_,_]].func
    if (test.res == defaultValue[B]) {  
      val t = deviceIndependentLowering
      Some(s.atPhase(t) {                        
        // TODO: repr still slices the input x by non-zero row (still operates on the sparse structure)
        // any way to operate only on the dense csr arrays? (pretty tricky, since rowFunc is defined on Interface[Vector[A]]))
        sparsematrix_maprowstovecnz(t(x), { a => transformBlockWithBound(t, repr, scala.List(e.fin -> a)) }, t(isRow))
      })
    }        
    else {
      warn("performance: mapRows function " + repr + " operates on zero values of sparse object " + findDefinition(x.asInstanceOf[Sym[Any]]).get.toString) // TODO: context for v
      None
    }          
  }
  
  // hacks for manifest
  // def isSparseMatAny[A:Manifest](x: Exp[Any]) = isSparseMat(x.asInstanceOf[Exp[DeliteCollection[A]]])
  // def asSparseMatAny[A:Manifest](x: Exp[Any]) = asSparseMat(x.asInstanceOf[Exp[DeliteCollection[A]]])
  
  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = d match {    
    case e@MatrixMapRowsToVec(x,rf,isrow) if (Config.optimize > 0 && isSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]])) =>
      specializeSparseMapRows(s, e, asSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]]), rf, isrow)(e.mA,e.mB).map(_.asInstanceOf[Exp[A]]) getOrElse super.onCreate(s,d)
    case Reflect(e@MatrixMapRowsToVec(x,rf,isrow), u, es) if (Config.optimize > 0 && isSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]])) =>
      reflectSpecialized(specializeSparseMapRows(s, e, asSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]]), rf, isrow)(e.mA,e.mB), u, es)(super.onCreate(s,d))    
      
    case _ => super.onCreate(s,d)
  }
  
  
}

/**
 *  Optimizations for composite MatrixOps operations.
 */

trait MatrixOpsExpOpt extends ppl.dsl.optila.matrix.DenseMatrixOpsExpOpt with MatrixOpsExp {
  this: OptiMLExp =>
  
   override def densematrix_numrows[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
     case Def(MatrixApplyRowIndices(x,y)) => y.length
     case Def(Reflect(MatrixApplyRowIndices(x,y),u,es)) => y.length
     case Def(IndexVector2Construct(lrows, lcols, f, fblk)) => lrows.length 
     case Def(Reflect(IndexVector2Construct(lrows, lcols, f, fblk),u,es)) => lrows.length 
     case _ => super.densematrix_numrows(x)
   }
   override def densematrix_numcols[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
     case Def(IndexVector2Construct(lrows, lcols, f, fblk)) => lcols.length
     case Def(Reflect(IndexVector2Construct(lrows, lcols, f, fblk),u,es)) => lcols.length
     case _ => super.densematrix_numcols(x)
   }
  // override def matrix_numrows[A:Manifest](x: Exp[Matrix[A]])(implicit ctx: SourceContext) = x match {
  //   //case Def(TrainingSetObjectFromMat(x,y)) => matrix_numrows(x) // TODO: move to TrainingSetOpsExpOpt ?
  //   case _ => super.matrix_numrows(x)
  // }
  // 
  // override def matrix_numcols[A:Manifest](x: Exp[Matrix[A]])(implicit ctx: SourceContext) = x match {
  //   //case Def(TrainingSetObjectFromMat(x,y)) => matrix_numcols(x) // TODO: move to TrainingSetOpsExpOpt ?
  //   case _ => super.matrix_numcols(x)
  // }
}

trait DenseMatrixOpsExpOpt extends ppl.dsl.optila.matrix.DenseMatrixOpsExpOpt {
  this: OptiMLExp =>
  
  override def densematrix_multiply[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = (x,y) match {
      //reflectPure(MatrixMultiplyVectorizedFromIndexVectors(fblk,gblk,lcols.length))
    case _ => super.densematrix_multiply(x,y)
  }  

}


trait ScalaGenMatrixOps extends ScalaGenEffect {
  val IR: MatrixOpsExp with FunctionBlocksExp
  import IR._

}

trait CudaGenMatrixOps extends CudaGenEffect {
  val IR: MatrixOpsExp with FunctionBlocksExp
  import IR._

}

trait OpenCLGenMatrixOps extends OpenCLGenBase {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenMatrixOps extends CGenBase {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
