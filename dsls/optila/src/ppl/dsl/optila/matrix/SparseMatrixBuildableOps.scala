package ppl.dsl.optila.matrix

import java.io.{PrintWriter}

import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, OpenCLGenBase, CGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.Util._

import ppl.dsl.optila._

/**
 * SparseMatrixBuildable abstracts the format used by Sparse Matrix construction from the end user.
 * OptiML is free to use different representations (potentially device-specific), while the user is
 * constrained from performing inefficient operations before or after sparse matrix construction.
 */
trait SparseMatrixBuildableOps extends Variables {
  this: OptiLA =>

  implicit def repToSparseMatBuildableOps[A:Manifest](x: Rep[SparseMatrixBuildable[A]]) = new SparseMatBuildableOpsCls(x)
  implicit def varToSparseMatBuildableOps[A:Manifest](x: Var[SparseMatrixBuildable[A]]) = new SparseMatBuildableOpsCls(readVar(x))  
  implicit def sparseMatBuildableToInterface[A:Manifest](lhs: Rep[SparseMatrixBuildable[A]]) = new MBuildableInterface[A](new SparseMatBuildableOpsCls[A](lhs))
  implicit def sparseMatBuildableVarToInterface[A:Manifest](lhs: Var[SparseMatrixBuildable[A]]) = new MBuildableInterface[A](new SparseMatBuildableOpsCls[A](readVar(lhs)))
  
  implicit def sparseMatrixBuilder[A:Manifest](implicit ctx: SourceContext) = new MatrixBuilder[A,SparseMatrixBuildable[A],SparseMatrix[A]] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]): Rep[SparseMatrixBuildable[A]] = {
      Matrix.sparse[A](numRows, numCols) // COO
    }
    def toBuildableIntf(x: Rep[SparseMatrixBuildable[A]]): Interface[MatrixBuildable[A]] = sparseMatBuildableToInterface(x)
    def finalizer(x: Rep[SparseMatrixBuildable[A]]): Rep[SparseMatrix[A]] = x.finish // COO -> CSR
    def toIntf(x: Rep[SparseMatrix[A]]): Interface[Matrix[A]] = sparseMatToInterface(x)
  }  

  object SparseMatrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = sparsematrix_obj_new(numRows, numCols)
  }

  class SparseMatBuildableOpsCls[A:Manifest](val elem: Rep[SparseMatrixBuildable[A]]) extends MatBuildableOpsCls[A] {
    type Self = SparseMatrixBuildable[A]
    def wrap(x: Rep[SparseMatrixBuildable[A]]): Interface[MatrixBuildable[A]] = sparseMatBuildableToInterface(x)
    type V[X] = SparseVector[X]
    type M[X] = SparseMatrixBuildable[X]    
    def mA: Manifest[A] = manifest[A]
    def toIntf[B:Manifest](x: Rep[M[B]]) = sparseMatBuildableToInterface(x)
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = sparseVecToInterface[B](x)        
    
    // accessors
    def nnz(implicit ctx: SourceContext) = sparsematrix_buildable_nnz(elem)
    def numRows(implicit ctx: SourceContext) = sparsematrix_buildable_numrows(x)
    def numCols(implicit ctx: SourceContext) = sparsematrix_buildable_numcols(x)
    def size(implicit ctx: SourceContext) = sparsematrix_buildable_size(x)    
    // def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = sparsematrix_buildable_apply(x,i,j)        
    
    // FIXME: see MatrixBuildableOps.scala
    protected def _numRows(implicit ctx: SourceContext) = sparsematrix_buildable_numrows(x)
    protected def _numCols(implicit ctx: SourceContext) = sparsematrix_buildable_numcols(x)    
    
    // data operations
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = sparsematrix_buildable_update(x,i,j,y)
    def insertRow(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = sparsematrix_buildable_insertrow(x,pos,y)
    def insertAllRows(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = sparsematrix_buildable_insertallrows(x,pos,y)
    def insertCol(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = sparsematrix_buildable_insertcol(x,pos,y)
    def insertAllCols(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = sparsematrix_buildable_insertallcols(x,pos,y)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = sparsematrix_buildable_removerows(x,pos,len)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = sparsematrix_buildable_removecols(x,pos,len)    
    
    // convert to output
    def finish: Rep[SparseMatrix[A]] = sparsematrix_buildable_finish(x)
  }
  
  // object defs
  def sparsematrix_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[SparseMatrixBuildable[A]]  
  
  // class defs
  def sparsematrix_buildable_apply[A:Manifest](x: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def sparsematrix_buildable_numrows[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparsematrix_buildable_numcols[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparsematrix_buildable_nnz[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[Int]

  def sparsematrix_buildable_update[A:Manifest](x: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_insertrow[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_insertallrows[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_insertcol[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_insertallcols[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_removerows[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_removecols[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  
  def sparsematrix_buildable_size[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparsematrix_buildable_finish[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[SparseMatrix[A]]
}

trait SparseMatrixBuildableCompilerOps extends SparseMatrixBuildableOps {
  this: OptiLA =>
  
  def sparsematrix_buildable_set_numrows[A:Manifest](x: Rep[SparseMatrixBuildable[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_set_numcols[A:Manifest](x: Rep[SparseMatrixBuildable[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_buildable_set_nnz[A:Manifest](x: Rep[SparseMatrixBuildable[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait SparseMatrixBuildableOpsExp extends SparseMatrixBuildableCompilerOps with DeliteCollectionOpsExp with VariablesExp {
  this: SparseMatrixBuildableImplOps with OptiLAExp  =>
  
  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class SparseMatrixBuildableNumRows[A:Manifest](x: Exp[SparseMatrixBuildable[A]]) extends DefWithManifest[A,Int] 
  case class SparseMatrixBuildableNumCols[A:Manifest](x: Exp[SparseMatrixBuildable[A]]) extends DefWithManifest[A,Int]
  case class SparseMatrixBuildableNNZ[A:Manifest](x: Exp[SparseMatrixBuildable[A]]) extends DefWithManifest[A,Int]
  case class SparseMatrixBuildableSetNumRows[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class SparseMatrixBuildableSetNumCols[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class SparseMatrixBuildableSetNNZ[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
      
  /////////////////////////////////////
  // implemented via kernel embedding

  case class SparseMatrixBuildableApply[A:Manifest](x: Exp[SparseMatrixBuildable[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(sparsematrix_buildable_apply_impl(x, i, j)))

  case class SparseMatrixBuildableUpdate[A:Manifest](x: Exp[SparseMatrixBuildable[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_update_impl(x,i,j,y)))

  case class SparseMatrixBuildableAppend[A:Manifest](x: Exp[SparseMatrixBuildable[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_append_impl(x,i,j,y)))
  
  case class SparseMatrixBuildableInsertRow[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Vector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_insertrow_impl(x,pos,y)))
    
  case class SparseMatrixBuildableInsertAllRows[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Matrix[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_insertallrows_impl(x,pos,y)))
    
  case class SparseMatrixBuildableInsertCol[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Vector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_insertcol_impl(x,pos,y)))
    
  case class SparseMatrixBuildableInsertAllCols[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Matrix[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_insertallcols_impl(x,pos,y)))
    
  case class SparseMatrixBuildableRemoveRows[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_removerows_impl(x,pos,len)))
    
  case class SparseMatrixBuildableRemoveCols[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_buildable_removecols_impl(x,pos,len)))

//  case class SparseMatrixBuildableUpdateRow[A:Manifest](x: Exp[SparseMatrixBuildable[A]], row: Exp[Int], y: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(sparsematrix_buildable_updaterow_impl(x,row,y)))
  
    
  ////////////////////
  // object interface

  //def symsparsematrix_obj_new[A:Manifest](n: Exp[Int]) = reflectMutable(SymmetricSparseMatrixObjectNew[A](n))
  //def sparsematrix_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(SparseMatrixObjectNew[A](numRows, numCols)) //XXX


  ///////////////////
  // class interface

  def sparsematrix_buildable_numrows[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixBuildableNumRows(x))
  def sparsematrix_buildable_numcols[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixBuildableNumCols(x))
  def sparsematrix_buildable_nnz[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixBuildableNNZ(x))
  def sparsematrix_buildable_apply[A:Manifest](x: Exp[SparseMatrixBuildable[A]], i: Exp[Int], j: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SparseMatrixBuildableApply[A](x,i,j))
  def sparsematrix_buildable_update[A:Manifest](x: Exp[SparseMatrixBuildable[A]], i: Exp[Int], j: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableUpdate[A](x,i,j,y))
  def sparsematrix_buildable_append[A:Manifest](x: Exp[SparseMatrixBuildable[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) = reflectWrite(x)(SparseMatrixBuildableAppend[A](x,i,j,y))
  def sparsematrix_buildable_insertrow[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableInsertRow(x,pos,y))
  def sparsematrix_buildable_insertallrows[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableInsertAllRows(x,pos,y))
  def sparsematrix_buildable_insertcol[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableInsertCol(x,pos,y))
  def sparsematrix_buildable_insertallcols[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableInsertAllCols(x,pos,y))
  def sparsematrix_buildable_removerows[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableRemoveRows(x,pos,len))
  def sparsematrix_buildable_removecols[A:Manifest](x: Exp[SparseMatrixBuildable[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableRemoveCols(x,pos,len))

  //////////////////
  // internal

  def sparsematrix_buildable_size[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = x.numRows * x.numCols
  def sparsematrix_buildable_set_numrows[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableSetNumRows(x,newVal))
  def sparsematrix_buildable_set_numcols[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableSetNumCols(x,newVal))  
  def sparsematrix_buildable_set_nnz[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixBuildableSetNNZ(x,newVal))
  
  
  /////////////////////
  // delite collection
  
  def isSparseMatBuildable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[SparseMatrixBuildable[A]])  
  def asSparseMatBuildable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[SparseMatrixBuildable[A]]]
  
  /**
   * dc_apply should only be called on SparseMatrix (inputs to delite ops are SparseMatrix). 
   * dc_append and friends should only be called on SparseMatrixBuildable (allocs are SparseMatrixBuildable).
   * 
   * dc_copy and dc_alloc should be overridden by implementations of SparseMatrixBuildableOps (e.g. SparseMatrixCOOOps)
   */ 
   
  // override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
  //   // AKS TODO FIXME: this is wrong! we can't set the 2d size from just a 1d scalar.
  //   if (isSparseMatBuildable(x)) {
  //     val m = asSparseMatBuildable(x)
  //     sparsematrix_buildable_set_numrows(m, y/m.numCols)
  //     sparsematrix_buildable_set_numcols(m, y%m.numCols)
  //   }
  //   else super.dc_set_logical_size(x,y)        
  // }

  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isSparseMatBuildable(x)) {
      if (y != defaultValue[A]) { 
        val m = asSparseMatBuildable(x)
        sparsematrix_buildable_append(m,i/m.numCols,i%m.numCols,y)
        unit(true) 
      }
      else unit(false)
    }      
    else super.dc_append(x,i,y)        
  } 
    
  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (isSparseMatBuildable(x)) ParBuffer
    else super.dc_parallelization(x, hasConditions)
  }
  
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseMatrixBuildableNumRows(x) => reflectPure(SparseMatrixBuildableNumRows(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixBuildableNumCols(x) => reflectPure(SparseMatrixBuildableNumCols(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixBuildableNNZ(x) => reflectPure(SparseMatrixBuildableNNZ(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])

    // delite ops
    case e@SparseMatrixApply(x,i,j) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixApply(f(x),f(i),f(j))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    
    // reflected
    case Reflect(e@SparseMatrixBuildableNumRows(x), u, es) => reflectMirrored(Reflect(SparseMatrixBuildableNumRows(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseMatrixBuildableNumCols(x), u, es) => reflectMirrored(Reflect(SparseMatrixBuildableNumCols(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    case Reflect(e@SparseMatrixBuildableNNZ(x), u, es) => reflectMirrored(Reflect(SparseMatrixBuildableNNZ(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    case Reflect(e@SparseMatrixBuildableSetNumRows(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixBuildableSetNumRows(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixBuildableSetNumCols(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixBuildableSetNumCols(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(e@SparseMatrixBuildableSetNNZ(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixBuildableSetNNZ(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(e@SparseMatrixBuildableApply(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableApply(f(x),f(i),f(j))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))      
    case Reflect(e@SparseMatrixBuildableUpdate(x,i,j,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableUpdate(f(x),f(i),f(j),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseMatrixBuildableAppend(x,i,j,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableAppend(f(x),f(i),f(j),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseMatrixBuildableInsertRow(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableInsertRow(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))              
    case Reflect(e@SparseMatrixBuildableInsertAllRows(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableInsertAllRows(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))                  
    case Reflect(e@SparseMatrixBuildableInsertCol(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableInsertCol(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))                
    case Reflect(e@SparseMatrixBuildableInsertAllCols(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableInsertAllCols(f(x),f(y),f(z))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixBuildableRemoveRows(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableRemoveRows(f(x),f(y),f(z))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseMatrixBuildableRemoveCols(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixBuildableRemoveCols(f(x),f(y),f(z))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
    
}


trait ScalaGenSparseMatrixBuildableOps extends ScalaGenBase {
  val IR: SparseMatrixBuildableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case SparseMatrixBuildableNumRows(x)  => emitValDef(sym, quote(x) + "._numRows")
    case SparseMatrixBuildableNumCols(x)  => emitValDef(sym, quote(x) + "._numCols")
    case SparseMatrixBuildableNNZ(x)  => emitValDef(sym, quote(x) + "._nnz")
    case SparseMatrixBuildableSetNumRows(x,v) => emitValDef(sym, quote(x) + "._numRows = " + quote(v))
    case SparseMatrixBuildableSetNumCols(x,v) => emitValDef(sym, quote(x) + "._numCols = " + quote(v))
    case SparseMatrixBuildableSetNNZ(x,v) => emitValDef(sym, quote(x) + "._nnz = " + quote(v))    
    case _ => super.emitNode(sym, rhs)
  }  
}

trait CudaGenSparseMatrixBuildableOps extends CudaGenBase with CudaGenDataStruct {
  val IR: SparseMatrixBuildableOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait OpenCLGenSparseMatrixBuildableOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: SparseMatrixBuildableOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenSparseMatrixBuildableOps extends CGenBase {
  val IR: SparseMatrixBuildableOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}