package ppl.dsl.optila.vector

import java.io.PrintWriter
import scala.reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase, ScalaGenFat, CudaGenBase, CudaGenFat}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenericFatCodegen,GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._
import ppl.dsl.optila._

trait SparseVectorViewOps extends Base with OverloadHack { this: OptiLA =>

  implicit def repToSparseVectorViewOps[A:Manifest](x: Rep[SparseVectorView[A]]) = new SparseVectorViewOpsCls(x)
  implicit def varToSparseVectorViewOps[A:Manifest](x: Var[SparseVectorView[A]]) = new SparseVectorViewOpsCls(readVar(x))
  implicit def sparseViewToInterface[A:Manifest](lhs: Rep[SparseVectorView[A]]) = new VInterface(new SparseVectorViewOpsCls(lhs))
    
  object SparseVectorView {
    def apply[A:Manifest](x: Rep[SparseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]) = sparse_vectorview_obj_new(x,start,stride,length,isRow)
  }
  
  class SparseVectorViewOpsCls[A:Manifest](val elem: Rep[SparseVectorView[A]]) extends VecOpsCls[A] {
    type Self = SparseVectorView[A]
    type VA = SparseVector[A]
    
    def mA = manifest[A]
    def mVA = mV[A]
    def wrap(x: Rep[SparseVectorView[A]]) = sparseViewToInterface(x)
    def vaToOps(x: Rep[VA]) = vecToOps[A](x)
    def vaToIntf(x: Rep[VA]) = vecToIntf[A](x)
    def vaBuilder(implicit ctx: SourceContext) = vecBuilder[A]      
    // -- without generic defs
    type V[X] = SparseVector[X]
    type M[X] = SparseMatrix[X]
    type I[X] = SparseMatrixBuildable[X]    
    def vecToOps[B:Manifest](x: Rep[SparseVector[B]]) = repToSparseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[SparseVector[B]]): Interface[Vector[B]] = sparseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[SparseMatrix[B]]): Interface[Matrix[B]] = sparseMatToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = sparseVectorBuilder[B]
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = sparseMatrixBuilder[B]
    def mV[B:Manifest] = manifest[SparseVector[B]]
    def mM[B:Manifest] = manifest[SparseMatrix[B]]
    // -- end without generic defs
    
    // VectorOps
    def length(implicit ctx: SourceContext) = sparse_vectorview_length(elem)
    def isRow(implicit ctx: SourceContext) = sparse_vectorview_isrow(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = sparse_vectorview_apply(elem,n)
    def sort(implicit o: Ordering[A], ctx: SourceContext) = elem.Clone.sort    
    def t(implicit ctx: SourceContext) = sparse_vectorview_transpose(x)

    // view accessors
    def start(implicit ctx: SourceContext) = sparse_vectorview_start(elem)
    def stride(implicit ctx: SourceContext) = sparse_vectorview_stride(elem)    
    
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")        
    def mt()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")    
    def copyFrom(pos: Rep[Int], y: Rep[SparseVector[A]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")
    def insert(pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[SparseVector[A]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")
    def trim()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")
    def clear()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("SparseVectorViews cannot be updated")  
    
    // specializations
    def +(y: Rep[DenseVector[A]])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext) 
      = vector_plus[A,DenseVector[A]](wrap(elem),denseVecToInterface(y))(manifest[A],implicitly[Arith[A]],manifest[DenseVector[A]],denseVectorBuilder[A],ctx)                          
  } 
  
  // implemented by mixed in trait for a particular kind of SparseMatrix  
  def sparse_vectorview_obj_new[A:Manifest](x: Rep[SparseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[SparseVectorView[A]]
  def sparse_vectorview_apply[A:Manifest](x: Rep[SparseVectorView[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
    
  // implemented for all sparse vector views (implementations must have the expected struct fields)
  def sparse_vectorview_length[A:Manifest](x: Rep[SparseVectorView[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparse_vectorview_isrow[A:Manifest](x: Rep[SparseVectorView[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def sparse_vectorview_start[A:Manifest](x: Rep[SparseVectorView[A]])(implicit ctx: SourceContext): Rep[Int]
  def sparse_vectorview_stride[A:Manifest](x: Rep[SparseVectorView[A]])(implicit ctx: SourceContext): Rep[Int]  
  def sparse_vectorview_transpose[A:Manifest](x: Rep[SparseVectorView[A]])(implicit ctx: SourceContext): Rep[SparseVector[A]]
}

trait SparseVectorViewCompilerOps extends SparseVectorViewOps {
  this: OptiLACompiler =>
 
  def sparse_vectorview_source[A:Manifest](x: Rep[SparseVectorView[A]])(implicit ctx: SourceContext): Rep[SparseMatrix[A]]
}

trait SparseVectorViewOpsExp extends SparseVectorViewCompilerOps with DeliteCollectionOpsExp { this: OptiLAExp with SparseVectorViewImplOps =>
  case class SparseVectorViewLength[A:Manifest](x: Exp[SparseVectorView[A]]) extends DefWithManifest[A,Int]
  case class SparseVectorViewIsRow[A:Manifest](x: Exp[SparseVectorView[A]]) extends DefWithManifest[A,Boolean]
  case class SparseVectorViewSource[A:Manifest](x: Exp[SparseVectorView[A]]) extends DefWithManifest[A,SparseMatrix[A]]
  case class SparseVectorViewStart[A:Manifest](x: Exp[SparseVectorView[A]]) extends DefWithManifest[A,Int]
  case class SparseVectorViewStride[A:Manifest](x: Exp[SparseVectorView[A]]) extends DefWithManifest[A,Int]
  case class SparseVectorViewApply[A:Manifest](x: Exp[SparseVectorView[A]], n: Exp[Int]) 
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(sparse_vectorview_apply_impl(x,n)))
    
  def sparse_vectorview_length[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = SparseVectorViewLength(x)
  def sparse_vectorview_isrow[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = SparseVectorViewIsRow(x)
  def sparse_vectorview_source[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = SparseVectorViewSource(x)
  def sparse_vectorview_start[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = SparseVectorViewStart(x)
  def sparse_vectorview_stride[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = SparseVectorViewStride(x)
  def sparse_vectorview_apply[A:Manifest](x: Exp[SparseVectorView[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SparseVectorViewApply(x, n))
  def sparse_vectorview_transpose[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext): Exp[SparseVector[A]] = SparseVector[A](unit(0), !x.isRow) ++ x
  
  /////////////////////
  // delite collection
    
  def isSparseView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[SparseVectorView[A]])
  def asSparseView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[SparseVectorView[A]]]    
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isSparseView(x)) asSparseView(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isSparseView(x)) asSparseView(x).apply(n)
    else super.dc_apply(x,n)    
  }
    
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseVectorViewLength(x) => reflectPure(SparseVectorViewLength(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseVectorViewIsRow(x) => reflectPure(SparseVectorViewIsRow(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseVectorViewStart(x) => reflectPure(SparseVectorViewStart(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseVectorViewStride(x) => reflectPure(SparseVectorViewStride(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@SparseVectorViewSource(x) => reflectPure(SparseVectorViewSource(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@SparseVectorViewApply(x,n) => reflectPure(new { override val original = Some(f,e) } with SparseVectorViewApply(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])    
        
    case Reflect(e@SparseVectorViewLength(x), u, es) => reflectMirrored(Reflect(SparseVectorViewLength(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseVectorViewIsRow(x), u, es) => reflectMirrored(Reflect(SparseVectorViewIsRow(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseVectorViewStart(x), u, es) => reflectMirrored(Reflect(SparseVectorViewStart(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseVectorViewStride(x), u, es) => reflectMirrored(Reflect(SparseVectorViewStride(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseVectorViewSource(x), u, es) => reflectMirrored(Reflect(SparseVectorViewSource(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseVectorViewApply(x,n), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseVectorViewApply(f(x),f(n))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
}


trait SparseVectorViewOpsExpOpt extends SparseVectorViewOpsExp { this: OptiLAExp =>
  
  override def sparse_vectorview_length[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(SparseMatrixVView(m, start, stride, l, r)) => l
    case Def(s@Reflect(SparseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => l
    case _ => super.sparse_vectorview_length(x)
  }
  
  override def sparse_vectorview_isrow[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(SparseMatrixVView(m, start, stride, l, r)) => r
    case Def(s@Reflect(SparseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => r
    case _ => super.sparse_vectorview_isrow(x) 
  }  
  
  override def sparse_vectorview_start[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(SparseMatrixVView(m, start, stride, l, r)) => start
    case Def(s@Reflect(SparseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => start
    case _ => super.sparse_vectorview_start(x) 
  }  
  
  override def sparse_vectorview_stride[A:Manifest](x: Exp[SparseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(SparseMatrixVView(m, start, stride, l, r)) => stride
    case Def(s@Reflect(SparseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => stride
    case _ => super.sparse_vectorview_stride(x) 
  }  
  
}



trait BaseGenSparseVectorViewOps extends GenericFatCodegen {
  val IR: SparseVectorViewOpsExp
  import IR._
  
  // override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
  //     case SparseVectorViewApply(a, i) => Some((a,i))
  //     case _ => super.unapplySimpleIndex(e)
  //   }  
}

trait ScalaGenSparseVectorViewOps extends BaseGenSparseVectorViewOps with ScalaGenFat {
  val IR: SparseVectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case SparseVectorViewLength(x) => emitValDef(sym, quote(x) + "._length")
    case SparseVectorViewIsRow(x) => emitValDef(sym, quote(x) + "._isRow")
    case SparseVectorViewSource(x) => emitValDef(sym, quote(x) + "._source")
    case SparseVectorViewStart(x) => emitValDef(sym, quote(x) + "._start")
    case SparseVectorViewStride(x) => emitValDef(sym, quote(x) + "._stride")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenSparseVectorViewOps extends BaseGenSparseVectorViewOps with CudaGenFat {
  val IR: SparseVectorViewOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

  
