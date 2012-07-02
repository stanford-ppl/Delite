package ppl.dsl.optila.vector

import java.io.PrintWriter
import scala.reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase, ScalaGenFat, CudaGenBase, CudaGenFat, OpenCLGenFat}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenericFatCodegen,GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._
import ppl.dsl.optila._

trait DenseVectorViewOps extends Base with OverloadHack { this: OptiLA =>

  implicit def repToDenseVectorViewOps[A:Manifest](x: Rep[DenseVectorView[A]]) = new DenseVectorViewOpsCls(x)
  implicit def varToDenseVectorViewOps[A:Manifest](x: Var[DenseVectorView[A]]) = new DenseVectorViewOpsCls(readVar(x))
  implicit def denseViewToInterface[A:Manifest](lhs: Rep[DenseVectorView[A]]) = new VInterface(new DenseVectorViewOpsCls(lhs))
    
  object DenseVectorView {
    def apply[A:Manifest](x: Rep[Array[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]) = dense_vectorview_obj_new(x,start,stride,length,isRow)
  }
  
  class DenseVectorViewOpsCls[A:Manifest](val elem: Rep[DenseVectorView[A]]) extends VecOpsCls[A] {
    type Self = DenseVectorView[A]
    type VA = DenseVector[A]
    
    def mA = manifest[A]
    def mVA = mV[A]
    def vaToOps(x: Rep[VA]) = vecToOps[A](x)
    def vaToIntf(x: Rep[VA]) = vecToIntf[A](x)
    def vaBuilder(implicit ctx: SourceContext) = vecBuilder[A]          
    def wrap(x: Rep[DenseVectorView[A]]) = denseViewToInterface(x)
    // -- without generic defs
    type V[X] = DenseVector[X]
    type M[X] = DenseMatrix[X]
    type I[X] = DenseMatrix[X]    
    def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]): Interface[Matrix[B]] = denseMatToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = denseMatrixBuilder[B]
    def mV[B:Manifest] = manifest[DenseVector[B]]
    def mM[B:Manifest] = manifest[DenseMatrix[B]]
    // -- end without generic defs

    // VectorOps
    def length(implicit ctx: SourceContext) = dense_vectorview_length(elem)
    def isRow(implicit ctx: SourceContext) = dense_vectorview_isrow(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = dense_vectorview_apply(elem,n)
    def sort(implicit o: Ordering[A], ctx: SourceContext) = elem.Clone.sort 
    def t(implicit ctx: SourceContext) = dense_vectorview_transpose(x)
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = dense_vectorview_update(x,n,y)
    
    // view accessors
    def start(implicit ctx: SourceContext) = dense_vectorview_start(elem)
    def stride(implicit ctx: SourceContext) = dense_vectorview_stride(elem)    
    
    // specializations
    def *(y: Rep[SparseVector[A]])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext) 
      = vector_times[A,SparseVector[A]](wrap(elem),sparseVecToInterface(y))(manifest[A],implicitly[Arith[A]],manifest[SparseVector[A]],sparseVectorBuilder[A],ctx)            
    def *(y: Rep[SparseVectorView[A]])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext) 
      = vector_times[A,SparseVector[A]](wrap(elem),sparseViewToInterface(y))(manifest[A],implicitly[Arith[A]],manifest[SparseVector[A]],sparseVectorBuilder[A],ctx)                
        
    def mt()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")    
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")
    def insert(pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")
    def trim()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")
    def clear()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("DenseVectorViews cannot be updated")        
  } 
  
  def dense_vectorview_obj_new[A:Manifest](x: Rep[Array[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVectorView[A]]
  def dense_vectorview_length[A:Manifest](x: Rep[DenseVectorView[A]])(implicit ctx: SourceContext): Rep[Int]
  def dense_vectorview_isrow[A:Manifest](x: Rep[DenseVectorView[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def dense_vectorview_apply[A:Manifest](x: Rep[DenseVectorView[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def dense_vectorview_start[A:Manifest](x: Rep[DenseVectorView[A]])(implicit ctx: SourceContext): Rep[Int]
  def dense_vectorview_stride[A:Manifest](x: Rep[DenseVectorView[A]])(implicit ctx: SourceContext): Rep[Int]    
  def dense_vectorview_transpose[A:Manifest](x: Rep[DenseVectorView[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def dense_vectorview_update[A:Manifest](x: Rep[DenseVectorView[A]], n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  
  // def dense_vectorview_times_matrix(x: Rep[DenseVectorView[A]], y: Rep[Matrix[Int]]): Rep[DenseVector[Int]]
  // def dense_vectorview_flatmap[B:Manifest](x: Rep[DenseVectorView[A]], f: Rep[A] => Rep[DenseVector[B]]): Rep[DenseVector[B]]
}

trait DenseVectorViewOpsExp extends DenseVectorViewOps with DeliteCollectionOpsExp { this: OptiLAExp =>
  case class DenseVectorViewNew[A:Manifest](x: Exp[Array[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) extends DefWithManifest[A,DenseVectorView[A]]
  case class DenseVectorViewLength[A:Manifest](x: Exp[DenseVectorView[A]]) extends DefWithManifest[A,Int]
  case class DenseVectorViewIsRow[A:Manifest](x: Exp[DenseVectorView[A]]) extends DefWithManifest[A,Boolean]
  case class DenseVectorViewApply[A:Manifest](x: Exp[DenseVectorView[A]], n: Exp[Int]) extends DefWithManifest[A,A]  
  case class DenseVectorViewStart[A:Manifest](x: Exp[DenseVectorView[A]]) extends DefWithManifest[A,Int]
  case class DenseVectorViewStride[A:Manifest](x: Exp[DenseVectorView[A]]) extends DefWithManifest[A,Int]  
  case class DenseVectorViewUpdate[A:Manifest](x: Exp[DenseVectorView[A]], n: Exp[Int], y: Exp[A]) extends DefWithManifest[A,Unit]
  
  def dense_vectorview_obj_new[A:Manifest](x: Exp[Array[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) = DenseVectorViewNew(x,start,stride,length,isRow)
  def dense_vectorview_length[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext): Exp[Int] = DenseVectorViewLength(x)
  def dense_vectorview_isrow[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext): Exp[Boolean] = DenseVectorViewIsRow(x)
  def dense_vectorview_apply[A:Manifest](x: Exp[DenseVectorView[A]], n: Exp[Int])(implicit ctx: SourceContext): Exp[A] = DenseVectorViewApply(x,n)
  def dense_vectorview_start[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = DenseVectorViewStart(x)
  def dense_vectorview_stride[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = DenseVectorViewStride(x)  
  def dense_vectorview_update[A:Manifest](x: Exp[DenseVectorView[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorViewUpdate(x, n, y))
  def dense_vectorview_transpose[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext): Exp[DenseVector[A]] = DenseVector[A](unit(0), !x.isRow) ++ x
  
  /////////////////////
  // delite collection
    
  def isDenseView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[DenseVectorView[A]])
  def asDenseView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DenseVectorView[A]]]  
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDenseView(x)) asDenseView(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDenseView(x)) asDenseView(x).apply(n)
    else super.dc_apply(x,n)    
  }
    
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DenseVectorViewNew(x,s,str,l,r) => reflectPure(DenseVectorViewNew(f(x),f(s),f(str),f(l),f(r))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorViewLength(x) => reflectPure(DenseVectorViewLength(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorViewIsRow(x) => reflectPure(DenseVectorViewIsRow(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorViewApply(x,n) => reflectPure(DenseVectorViewApply(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorViewStart(x) => reflectPure(DenseVectorViewStart(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorViewStride(x) => reflectPure(DenseVectorViewStride(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])        
        
    case Reflect(e@DenseVectorViewNew(x,s,str,l,r), u, es) => reflectMirrored(Reflect(DenseVectorViewNew(f(x),f(s),f(str),f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorViewLength(x), u, es) => reflectMirrored(Reflect(DenseVectorViewLength(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorViewIsRow(x), u, es) => reflectMirrored(Reflect(DenseVectorViewIsRow(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorViewStart(x), u, es) => reflectMirrored(Reflect(DenseVectorViewStart(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorViewStride(x), u, es) => reflectMirrored(Reflect(DenseVectorViewStride(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))        
    case Reflect(e@DenseVectorViewApply(x,n), u, es) => reflectMirrored(Reflect(DenseVectorViewApply(f(x),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorViewUpdate(x,n,y), u, es) => reflectMirrored(Reflect(DenseVectorViewUpdate(f(x),f(n),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
}

trait DenseVectorViewOpsExpOpt extends DenseVectorViewOpsExp { this: OptiLAExp =>
  
  override def dense_vectorview_length[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(DenseMatrixVView(m, start, stride, l, r)) => l
    // case Def(MatrixGetRow(m,i)) => m.numCols
    // case Def(MatrixGetCol(m,i)) => m.numRows
    case Def(s@Reflect(DenseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => l
    // case Def(s@Reflect(MatrixGetRow(m,i), u, es)) if context.contains(s) => m.numCols 
    // case Def(s@Reflect(MatrixGetCol(m,i), u, es)) if context.contains(s) => m.numRows
    case _ => super.dense_vectorview_length(x) //throw new RuntimeException("could not resolve type of " + findDefinition(x.asInstanceOf[Sym[DenseVectorView[A]]]).get.rhs)
  }  
  
  override def dense_vectorview_isrow[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(DenseMatrixVView(m, start, stride, l, r)) => r
    // case Def(MatrixGetRow(m,i)) => Const(true)
    // case Def(MatrixGetCol(m,i)) => Const(false)
    case Def(s@Reflect(DenseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => r
    // case Def(s@Reflect(MatrixGetRow(m,i), u, es)) if context.contains(s) => Const(true)
    // case Def(s@Reflect(MatrixGetCol(m,i), u, es)) if context.contains(s) => Const(false)
    case _ => super.dense_vectorview_isrow(x) //throw new RuntimeException("could not resolve type of " + findDefinition(x.asInstanceOf[Sym[DenseVectorView[A]]]).get.rhs) 
  }
  
  // and this one also helps in the example:
  def dense_vectorview_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Option[Exp[A]] = x match {
    case Def(DenseMatrixVView(m, start, stride, l, r)) => Some(m.dcApply(start + n*stride))
    case Def(s@Reflect(DenseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => Some(m.dcApply(start + n*stride))
    // case Def(MatrixGetRow(m,i)) => Some(m(i,n))
    // case Def(MatrixGetCol(m,j)) => Some(m(n,j))
    // case Def(s@Reflect(MatrixGetRow(m,i), u, es)) if context.contains(s) => Some(m(i,n))
    // case Def(s@Reflect(MatrixGetCol(m,j), u, es)) if context.contains(s) => Some(m(n,j))
    case _ => None
  }
  
  override def dense_vectorview_apply[A:Manifest](x: Exp[DenseVectorView[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    dense_vectorview_optimize_apply(x.asInstanceOf[Exp[DeliteCollection[A]]],n) getOrElse super.dense_vectorview_apply(x,n)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    dense_vectorview_optimize_apply(x,n) getOrElse super.dc_apply(x,n)
  }
}

trait BaseGenDenseVectorViewOps extends GenericFatCodegen {
  val IR: DenseVectorViewOpsExp
  import IR._
  
  // override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
  //     case DenseVectorViewApply(a, i) => Some((a,i))
  //     case _ => super.unapplySimpleIndex(e)
  //   }  
}

trait ScalaGenDenseVectorViewOps extends BaseGenDenseVectorViewOps with ScalaGenFat {
  val IR: DenseVectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v@DenseVectorViewNew(x,start,stride,length,isRow) => emitValDef(sym, "new " + remap("DenseVectorView[" + remap(v.mA) + "]") + "(" + quote(x) + "," + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ")")
    case DenseVectorViewApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case DenseVectorViewUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case DenseVectorViewLength(x)    => emitValDef(sym, quote(x) + "._length")
    case DenseVectorViewIsRow(x)     => emitValDef(sym, quote(x) + "._isRow")
    case DenseVectorViewStart(x) => emitValDef(sym, quote(x) + "._start")
    case DenseVectorViewStride(x) => emitValDef(sym, quote(x) + "._stride")    
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDenseVectorViewOps extends BaseGenDenseVectorViewOps with CudaGenFat {
  val IR: DenseVectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    //TODO: Allow this to only kernels (not helper functions)
    case DenseVectorViewNew(x,start,stride,length,isRow) => {
      if(!processingHelperFunc) stream.println(remap(sym.tp) + " " + quote(sym) + "(" + quote(x) + "," + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ");")
      else throw new GenerationFailedException("CudaGen: DenseVectorViewNew cannot be used in helper functions.")
    }
    case DenseVectorViewApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case DenseVectorViewUpdate(x,n,y) => stream.println(quote(x) + ".update(" + quote(n) + "," + quote(y) + ");\n")
    case DenseVectorViewLength(x)    => emitValDef(sym, quote(x) + ".length")
    case DenseVectorViewIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenDenseVectorViewOps extends BaseGenDenseVectorViewOps with OpenCLGenFat {
  val IR: DenseVectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DenseVectorViewNew(x,start,stride,length,isRow) => {
      if(!processingHelperFunc) stream.println(remap(sym.tp) + " " + quote(sym) + "(" + quote(x) + "," + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ");")
      else throw new GenerationFailedException("OpenCLGen: DenseVectorViewNew cannot be used in helper functions.")
    }
    case DenseVectorViewApply(x,n) => emitValDef(sym, remap(x.tp) + "_apply(" + quote(x) + "," + quote(n) + ")")
    case DenseVectorViewUpdate(x,n,y) => stream.println(remap(x.tp) + "_update(" + quote(x) + "," + quote(n) + "," + quote(y) + ");\n")
    case DenseVectorViewLength(x)    => emitValDef(sym, quote(x) + ".length")
    case DenseVectorViewIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
    case _ => super.emitNode(sym, rhs)
  }
}

  
