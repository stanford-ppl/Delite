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

/*
 * Sparse matrix ops design notes
 * =================================
 * 
 * Currently, if a sparse matrix op is not specialized, it falls back to generic Matrix ops, which
 * are inefficient for sparse structures because they operate on the logical matrices instead of just
 * the non-zero values. If a parallel op falls back to the generic matrix ops, it is implemented to
 * read from CSR matrices (input is a SparseMatrix) and write to COO matrices (intermediate output is
 * a SparseMatrixBuildable) before finally being converted back to a CSR at the end of the op.
 * 
 * For efficient sparse performance, we will want to specialize ops on sparse structures. The most
 * straightforward way is to override the ops in SparseMatrixOps on a case-by-case basis (and possibly
 * with different overrides for different arguments, e.g. sparse-sparse, sparse-dense, etc.). 
 * The following idea describes a method for doing that at the Delite op level, to retain some genericity:
 * 
 * ==================================
 * 
 * Specialization via Transformation  
 * 
 * Is it possible to go from
 *  val t1 = m map { e => foo(e) }
 * 
 * to:
 *  val x1 = foo(0)
 *  val x2 = m.mapNZWithDefault(e => foo(e), foo(0)) // can do different things if foo(0) == 0 or not
 * 
 * not by construction, but strictly by analyzing/optimizing the original map operation? in other words, can we handle this at the delite op level?
 * 
 * Using ForwardTransformers:
 * 
 * let's say we look at a DeliteOpMap(intf, f), where intf: Interface[Matrix[A]]. we can dispatch on intf.mM[A] to see if it's a manifest for a SparseMatrix[A].
 *  if it is, we can transform it as above!
 * 
 * same thing with Zip:
 * (do different things if just one of the inputs is sparse)
 * DeliteOpZipWith(intfA, intfB, f) if intfA == SparseVector && intfB == SparseVector => 
 *  test the zip function - this is essentially recovering the operator info!: (how would we get nonzero values to test with? nonDefaultValue[A]? sound?)
 *    zip(a,0) && zip(0,b) = 0 => apply zip function only on intersection(nz(a),nz(b))
 *    zip(a,0) = 0 => apply zip function only on nz(b)
 *    zip(0,b) = 0 => apply zip function only on nz(a)
 *    zip(0,0) = 0 => apply zip function only on union(nz(a),nz(b))
 *  if testing the zip function is infeasible, can we pass the operator info along with the zip in a context parameter? e.g. + is a zip(bothArgsMustBeZero) kind of thing
 *      
 * SparseMatrix should:
 *   1) use the transformation trick to lower generic maps and zips to their more efficient version if they're sparse [thus, reusing interface!]
 *   2) add a finalize method to MatrixBuilder: 
 *      - alloc constructs the output as a COO 
 *      - finalize transforms it and returns it as an immutable CSR (as discussed above)
 *      - zips only occur on CSRs!
 *      use the type system to force finalize to be called. have alloc return a Rep[NeedToBeFinalized], and finalize return a Rep[MB]
 * 
 * if we do the transformations right, there should be no fallback case! (so the collectelem interface really didn't need to change after all!)
 * thus, all mutable ops in SparseMatrix can just throw an exception; we can explicitly construct COO in our sparse constructors, and everything would stay CSR after that
 *    - this is actually nice because it guarantees the high performance transformation happened. any case where it couldn't?
 *    - theoretically dc ops for sparsevector/sparsematrix could all throw "should have been transformed" runtime exceptions
 *    ??? there is a lot of stuff in vector/matrix impl that relies on mutability, particular alloc -> update to implement sequential ops. that stuff would all be broken.
 *      - this stuff would be "ok", not great, if it was allocated as COO and "somehow" auto-converted to CSR upon return. override unsafeImmutable to convert return?
 *      - the actual operations would still be quite slow unless they were overridden directly inside sparse vector/matrix, because we can't auto-specialize them
 * 
 * Note that this may interfere with fusion, since in the lowering process the optimized op will have write effects to set data fields. But surely we can fix fusion to handle this.
 * 
 */

trait SparseMatrixOps extends Variables {
  this: OptiLA =>
  
  implicit def repToSparseMatOps[A:Manifest](x: Rep[SparseMatrix[A]]) = new SparseMatOpsCls(x)
  implicit def varToSparseMatOps[A:Manifest](x: Var[SparseMatrix[A]]) = new SparseMatOpsCls(readVar(x))  
  implicit def sparseMatToInterface[A:Manifest](lhs: Rep[SparseMatrix[A]]) = new MInterface[A](new SparseMatOpsCls[A](lhs))
  implicit def sparseMatVarToInterface[A:Manifest](lhs: Var[SparseMatrix[A]]) = new MInterface[A](new SparseMatOpsCls[A](readVar(lhs)))
  
  class SparseMatOpsCls[A:Manifest](val elem: Rep[SparseMatrix[A]]) extends MatOpsCls[A] { 
    type M[X] = SparseMatrix[X]
    type V[X] = SparseVector[X]
    type View[X] = SparseVectorView[X]
    type I[X] = SparseMatrixBuildable[X]
    type Self = SparseMatrix[A]
    def wrap(x: Rep[SparseMatrix[A]]): Interface[Matrix[A]] = sparseMatToInterface(x)        
    def mA: Manifest[A] = manifest[A]
    def mM[B:Manifest]: Manifest[M[B]] = manifest[SparseMatrix[B]]    
    def mI[B:Manifest]: Manifest[I[B]] = manifest[SparseMatrixBuildable[B]]    
    def matToOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B] = repToSparseMatOps[B](x)
    def matToIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]] = sparseMatToInterface[B](x)        
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = sparseMatrixBuilder[B]            
    def mV[B:Manifest]: Manifest[V[B]] = manifest[SparseVector[B]]
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = sparseVecToInterface[B](x)        
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = sparseVectorBuilder[B]
    def viewToIntf[B:Manifest](x: Rep[View[B]]) = sparseViewToInterface(x)
    
    // delite collection
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] = sparsematrix_apply(x,n/x.numCols,n%x.numCols)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = sparsematrix_update(x,n/x.numCols,n%x.numCols,y)
    
    // accessors
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = sparsematrix_apply(x,i,j)
    def numRows(implicit ctx: SourceContext) = sparsematrix_numrows(x)
    def numCols(implicit ctx: SourceContext) = sparsematrix_numcols(x)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = sparsematrix_vview(x,start,stride,length,isRow)
    def nnz(implicit ctx: SourceContext) = sparsematrix_nnz(elem)
        
    // not supported by interface right now
    def *(y: Rep[MA])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = sparsematrix_multiply(x,y)
    def inv(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = sparsematrix_inverse(x)        
  }
  
  // class defs
  def sparsematrix_apply[A:Manifest](x: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def sparsematrix_update[A:Manifest](x: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_numrows[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparsematrix_numcols[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparsematrix_vview[A:Manifest](x: Rep[SparseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[SparseVectorView[A]] 
  def sparsematrix_nnz[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  
  def sparsematrix_multiply[A:Manifest:Arith](x: Rep[SparseMatrix[A]], y: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[SparseMatrix[A]]
  def sparsematrix_inverse[A:Manifest](x: Rep[SparseMatrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[SparseMatrix[Double]]  
}

trait SparseMatrixCompilerOps extends SparseMatrixOps {
  this: OptiLA =>
  
  def sparsematrix_set_numrows[A:Manifest](x: Rep[SparseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_set_numcols[A:Manifest](x: Rep[SparseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_set_nnz[A:Manifest](x: Rep[SparseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait SparseMatrixOpsExp extends SparseMatrixCompilerOps with DeliteCollectionOpsExp with VariablesExp {
  this: SparseMatrixImplOps with OptiLAExp  =>
  
  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class SparseMatrixNumRows[A:Manifest](x: Exp[SparseMatrix[A]]) extends DefWithManifest[A,Int] 
  case class SparseMatrixNumCols[A:Manifest](x: Exp[SparseMatrix[A]]) extends DefWithManifest[A,Int]
  case class SparseMatrixNNZ[A:Manifest](x: Exp[SparseMatrix[A]]) extends DefWithManifest[A,Int]
  case class SparseMatrixSetNumRows[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class SparseMatrixSetNumCols[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class SparseMatrixSetNNZ[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
    
  /////////////////////////////////////
  // implemented via kernel embedding

  case class SparseMatrixVView[A:Manifest](x: Exp[SparseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])
    extends DeliteOpSingleWithManifest[A,SparseVectorView[A]](reifyEffectsHere(sparsematrix_vview_impl(x, start, stride, length, isRow)))

  case class SparseMatrixApply[A:Manifest](x: Exp[SparseMatrix[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(sparsematrix_apply_impl(x, i, j)))
    
  case class SparseMatrixUpdate[A:Manifest](x: Exp[SparseMatrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(sparsematrix_update_impl(x, i, j, y)))

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  // case class SparseMatrixEquals[A:Manifest](x: Exp[SparseMatrix[A]], y: Exp[SparseMatrix[A]])
  //   extends DeliteOpSingleTask(reifyEffectsHere(sparsematrix_equals_impl[A](x,y)))
    
  // case class SparseMatrixInverse[A:Manifest](x: Exp[SparseMatrix[A]])(implicit val conv: Exp[A] => Exp[Double])
  //   extends DeliteOpSingleWithManifest[A,SparseMatrix[Double]](reifyEffectsHere(sparsematrix_inverse_impl(x))) 
  //   
  // case class SparseMatrixMultiply[A:Manifest:Arith](x: Exp[SparseMatrix[A]], y: Exp[SparseMatrix[A]])
  //   extends DeliteOpSingleWithManifest[A,SparseMatrix[A]](reifyEffectsHere(sparsematrix_multiply_impl(x,y))) {
  //   
  //   val a = implicitly[Arith[A]]
  // }
  
  ///////////////////
  // class interface

  def sparsematrix_apply[A:Manifest](x: Exp[SparseMatrix[A]], i: Exp[Int], j: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SparseMatrixApply[A](x,i,j))
  def sparsematrix_update[A:Manifest](x: Exp[SparseMatrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixUpdate[A](x,i,j,y))
  def sparsematrix_numrows[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixNumRows(x))
  def sparsematrix_numcols[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixNumCols(x))
  def sparsematrix_nnz[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixNNZ(x))
  def sparsematrix_vview[A:Manifest](x: Exp[SparseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(SparseMatrixVView(x,start,stride,length,isRow))

  def sparsematrix_multiply[A:Manifest:Arith](x: Exp[SparseMatrix[A]], y: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = {
    throw new UnsupportedOperationException("tbd")
    //reflectPure(SparseMatrixMultiply(x,y))
  }
  def sparsematrix_inverse[A:Manifest](x: Exp[SparseMatrix[A]])(implicit conv: Exp[A] => Exp[Double], ctx: SourceContext) = {
    throw new UnsupportedOperationException("tbd")
    //reflectPure(SparseMatrixInverse(x))
  }  
  
  //////////////////
  // internal

  def sparsematrix_set_numrows[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixSetNumRows(x,newVal))
  def sparsematrix_set_numcols[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixSetNumCols(x,newVal))  
  def sparsematrix_set_nnz[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixSetNNZ(x,newVal))
  
    
  /////////////////////
  // delite collection
  
  def isSparseMat[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[SparseMatrix[A]])  
  def asSparseMat[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[SparseMatrix[A]]]
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isSparseMat(x)) asSparseMat(x).size
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isSparseMat(x)) asSparseMat(x).dcApply(n)
    else super.dc_apply(x,n)    
  }  
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseMatrixNumRows(x) => reflectPure(SparseMatrixNumRows(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixNumCols(x) => reflectPure(SparseMatrixNumCols(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixNNZ(x) => reflectPure(SparseMatrixNNZ(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])

    // delite ops
    case e@SparseMatrixVView(x,s,str,l,r) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixVView(f(x),f(s),f(str),f(l),f(r))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixApply(x,i,j) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixApply(f(x),f(i),f(j))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    // case e@SparseMatrixInverse(x) => reflectPure(new {override val original = Some(f,e) } with SparseMatrixInverse(f(x))(e.mA,e.conv))(mtype(manifest[A]),implicitly[SourceContext])      
    // case e@SparseMatrixMultiply(x,y) => reflectPure(new {override val original = Some(f,e) } with SparseMatrixMultiply(f(x),f(y))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@SparseMatrixTimesVector(x,y) => reflectPure(new {override val original = Some(f,e) } with SparseMatrixTimesVector(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    
    // reflected
    case Reflect(e@SparseMatrixNumRows(x), u, es) => reflectMirrored(Reflect(SparseMatrixNumRows(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseMatrixNumCols(x), u, es) => reflectMirrored(Reflect(SparseMatrixNumCols(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    case Reflect(e@SparseMatrixNNZ(x), u, es) => reflectMirrored(Reflect(SparseMatrixNNZ(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    case Reflect(e@SparseMatrixSetNumRows(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixSetNumRows(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixSetNumCols(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixSetNumCols(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(e@SparseMatrixSetNNZ(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixSetNNZ(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(e@SparseMatrixVView(x,s,str,l,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixVView(f(x),f(s),f(str),f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))               
    case Reflect(e@SparseMatrixApply(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixApply(f(x),f(i),f(j))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))      
    // case Reflect(e@SparseMatrixInverse(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixInverse(f(x))(e.mA,e.conv), mapOver(f,u), f(es)))(mtype(manifest[A]))          
    // case Reflect(e@SparseMatrixMultiply(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixMultiply(f(x),f(y))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))         
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
    
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  /*
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    // case SparseMatrixMultiply(a,b) => Nil
    //case SparseMatrixTimesVector(a,v) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    // case SparseMatrixMultiply(a,b) => Nil
    //case SparseMatrixTimesVector(a,v) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    // case SparseMatrixMultiply(a,b) => Nil
    //case SparseMatrixTimesVector(a,v) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    // case SparseMatrixMultiply(a,b) => Nil
    //case SparseMatrixTimesVector(a,v) => Nil
    case _ => super.copySyms(e)
  } 
  */
}

/**
 *  Optimizations for composite SparseMatrixOps operations.
 */

trait SparseMatrixOpsExpOpt extends SparseMatrixOpsExp {
  this: SparseMatrixImplOps with OptiLAExp =>

  // override def sparsematrix_equals[A:Manifest](x: Exp[SparseMatrix[A]], y: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = (x, y) match {
  //   case (a,b) if (a == b) => unit(true) // same symbol
  //   case _ => super.sparsematrix_equals(x,y)
  // }

  // override def sparsematrix_numrows[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = x match {
  //   case Def(s@Reflect(SparseMatrixObjectNew(rows,cols), u, es)) if context.contains(s) => rows // only if not modified! // TODO: check writes
  //   case Def(SparseMatrixObjectNew(rows,cols)) => rows
  //   case _ => super.sparsematrix_numrows(x)
  // }
  // 
  // override def sparsematrix_numcols[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = x match {
  //   case Def(s@Reflect(SparseMatrixObjectNew(rows,cols), u, es)) if context.contains(s) => cols // only if not modified! // TODO: check writes
  //   case Def(SparseMatrixObjectNew(rows,cols)) => cols
  //   case _ => super.sparsematrix_numcols(x)
  // }  
}


trait ScalaGenSparseMatrixOps extends ScalaGenBase {
  val IR: SparseMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case SparseMatrixNumRows(x)  => emitValDef(sym, quote(x) + "._numRows")
    case SparseMatrixNumCols(x)  => emitValDef(sym, quote(x) + "._numCols")
    case SparseMatrixNNZ(x)  => emitValDef(sym, quote(x) + "._nnz")
    case SparseMatrixSetNumRows(x,v) => emitValDef(sym, quote(x) + "._numRows = " + quote(v))
    case SparseMatrixSetNumCols(x,v) => emitValDef(sym, quote(x) + "._numCols = " + quote(v))
    case SparseMatrixSetNNZ(x,v) => emitValDef(sym, quote(x) + "._nnz = " + quote(v))
    case _ => super.emitNode(sym, rhs)
  }  
}

trait CudaGenSparseMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: SparseMatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait OpenCLGenSparseMatrixOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: SparseMatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenSparseMatrixOps extends CGenBase {
  val IR: SparseMatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
