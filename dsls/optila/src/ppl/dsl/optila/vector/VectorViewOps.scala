package ppl.dsl.optila.vector

import java.io.PrintWriter
import scala.reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase, ScalaGenFat, CudaGenBase, CudaGenFat}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenericFatCodegen,GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optila.{Vector, DenseVector, VectorView, Matrix, DenseMatrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

trait VectorViewOps extends Base with OverloadHack { this: OptiLA =>

  implicit def repToVectorViewVecOps[A:Manifest](x: Rep[VectorView[A]]) = new VectorViewVecOpsCls(x)
  implicit def varToVectorViewVecOps[A:Manifest](x: Var[VectorView[A]]) = new VectorViewVecOpsCls(readVar(x))
  implicit def vectorViewToInterface[A:Manifest](lhs: Rep[VectorView[A]]) = new VInterface(new VectorViewVecOpsCls(lhs))
    
  object VectorView {
    def apply[A:Manifest](x: Rep[Array[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]) = vectorview_obj_new(x,start,stride,length,isRow)
  }
  
  class VectorViewVecOpsCls[A:Manifest](val elem: Rep[VectorView[A]]) extends VecOpsCls[A] {
    // type VA = VectorView
    // def toOps(x: Rep[VectorView[A]]) = repToVectorViewVecOps(x)
    // def toIntf(x: Rep[VectorView[A]]) = vectorViewToInterface(x)
    // def builder: VectorBuilder[Int,VectorView] = vectorViewVectorBuilder
    def mA = manifest[A]
    //def mVA = manifest[VectorView]
    
    type V[X] = DenseVector[X]
    type M[X] = DenseMatrix[X]
    type Self = VectorView[A]
    def wrap(x: Rep[VectorView[A]]) = vectorViewToInterface(x)
    def toOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]): Interface[Matrix[B]] = denseMatToInterface(x)
    def builder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,M[B]] = denseMatrixBuilder[B]
    def mV[B:Manifest] = manifest[DenseVector[B]]
    def mM[B:Manifest] = manifest[DenseMatrix[B]]

    // VectorOps
    def length(implicit ctx: SourceContext) = vectorview_length(elem)
    def isRow(implicit ctx: SourceContext) = vectorview_isrow(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = vectorview_apply(elem,n)
    def sort(implicit o: Ordering[A], ctx: SourceContext) = elem.Clone.sort    
    def t(implicit ctx: SourceContext) = vectorview_transpose(x)
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = vectorview_update(x,n,y)
    
    // generic
    type VPLUSR = DenseVector[A]
    val mVPLUSR = manifest[VPLUSR]
    def vplusBuilder(implicit ctx: SourceContext) = denseVectorBuilder[A]
    def vplusToIntf(x: Rep[VPLUSR]) = denseVecToInterface(x)
    
    type VMINUSR = DenseVector[A]
    val mVMINUSR = manifest[VMINUSR]
    def vminusBuilder(implicit ctx: SourceContext) = denseVectorBuilder[A]
    def vminusToIntf(x: Rep[VMINUSR]) = denseVecToInterface(x)    
    
    type VTIMESR = DenseVector[A]
    val mVTIMESR = manifest[VTIMESR]
    def vtimesBuilder(implicit ctx: SourceContext) = denseVectorBuilder[A]
    def vtimesToIntf(x: Rep[VTIMESR]) = denseVecToInterface(x)        
        
    def mt()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")    
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def insert(pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def trim()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def clear()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("VectorViews cannot be updated")        
  } 
  
  def vectorview_obj_new[A:Manifest](x: Rep[Array[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[VectorView[A]]
  def vectorview_length[A:Manifest](x: Rep[VectorView[A]])(implicit ctx: SourceContext): Rep[Int]
  def vectorview_isrow[A:Manifest](x: Rep[VectorView[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def vectorview_apply[A:Manifest](x: Rep[VectorView[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def vectorview_transpose[A:Manifest](x: Rep[VectorView[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def vectorview_update[A:Manifest](x: Rep[VectorView[A]], n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  
  // def vectorview_times_matrix(x: Rep[VectorView[A]], y: Rep[Matrix[Int]]): Rep[DenseVector[Int]]
  // def vectorview_flatmap[B:Manifest](x: Rep[VectorView[A]], f: Rep[A] => Rep[DenseVector[B]]): Rep[DenseVector[B]]
}

trait VectorViewOpsExp extends VectorViewOps with DeliteCollectionOpsExp { this: OptiLAExp =>
  case class VectorViewNew[A:Manifest](x: Exp[Array[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) extends DefWithManifest[A,VectorView[A]]
  case class VectorViewLength[A:Manifest](x: Exp[VectorView[A]]) extends DefWithManifest[A,Int]
  case class VectorViewIsRow[A:Manifest](x: Exp[VectorView[A]]) extends DefWithManifest[A,Boolean]
  case class VectorViewApply[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int]) extends DefWithManifest[A,A]  
  case class VectorViewUpdate[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int], y: Exp[A]) extends DefWithManifest[A,Unit]
  
  def vectorview_obj_new[A:Manifest](x: Exp[Array[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) = VectorViewNew(x,start,stride,length,isRow)
  def vectorview_length[A:Manifest](x: Exp[VectorView[A]])(implicit ctx: SourceContext): Exp[Int] = VectorViewLength(x)
  def vectorview_isrow[A:Manifest](x: Exp[VectorView[A]])(implicit ctx: SourceContext): Exp[Boolean] = VectorViewIsRow(x)
  def vectorview_apply[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int])(implicit ctx: SourceContext): Exp[A] = VectorViewApply(x,n)
  def vectorview_update[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(VectorViewUpdate(x, n, y))
  def vectorview_transpose[A:Manifest](x: Exp[VectorView[A]])(implicit ctx: SourceContext): Exp[DenseVector[A]] = DenseVector[A](unit(0), !x.isRow) ++ x
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@VectorViewNew(x,s,str,l,r) => reflectPure(VectorViewNew(f(x),f(s),f(str),f(l),f(r))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorViewLength(x) => reflectPure(VectorViewLength(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorViewIsRow(x) => reflectPure(VectorViewIsRow(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorViewApply(x,n) => reflectPure(VectorViewApply(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
        
    case Reflect(e@VectorViewNew(x,s,str,l,r), u, es) => reflectMirrored(Reflect(VectorViewNew(f(x),f(s),f(str),f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorViewLength(x), u, es) => reflectMirrored(Reflect(VectorViewLength(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorViewIsRow(x), u, es) => reflectMirrored(Reflect(VectorViewIsRow(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorViewApply(x,n), u, es) => reflectMirrored(Reflect(VectorViewApply(f(x),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorViewUpdate(x,n,y), u, es) => reflectMirrored(Reflect(VectorViewUpdate(f(x),f(n),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
}

trait VectorViewOpsExpOpt extends VectorViewOpsExp { this: OptiLAExp =>
  
  override def vectorview_length[A:Manifest](x: Exp[VectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(DenseMatrixVView(m, start, stride, l, r)) => l
    case Def(MatrixGetRow(m,i)) => m.numCols
    case Def(MatrixGetCol(m,i)) => m.numRows
    case Def(s@Reflect(DenseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => l
    case Def(s@Reflect(MatrixGetRow(m,i), u, es)) if context.contains(s) => m.numCols 
    case Def(s@Reflect(MatrixGetCol(m,i), u, es)) if context.contains(s) => m.numRows
    case _ => super.vectorview_length(x) //throw new RuntimeException("could not resolve type of " + findDefinition(x.asInstanceOf[Sym[VectorView[A]]]).get.rhs)
  }  
  
  override def vectorview_isrow[A:Manifest](x: Exp[VectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(DenseMatrixVView(m, start, stride, l, r)) => r
    case Def(MatrixGetRow(m,i)) => Const(true)
    case Def(MatrixGetCol(m,i)) => Const(false)
    case Def(s@Reflect(DenseMatrixVView(m, start, stride, l, r), u, es)) if context.contains(s) => r
    case Def(s@Reflect(MatrixGetRow(m,i), u, es)) if context.contains(s) => Const(true)
    case Def(s@Reflect(MatrixGetCol(m,i), u, es)) if context.contains(s) => Const(false)
    case _ => super.vectorview_isrow(x) //throw new RuntimeException("could not resolve type of " + findDefinition(x.asInstanceOf[Sym[VectorView[A]]]).get.rhs) 
  }
  
  // and this one also helps in the example:
  def vectorview_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Option[Exp[A]] = x match {
    case Def(MatrixGetRow(m,i)) => Some(m(i,n))
    case Def(MatrixGetCol(m,j)) => Some(m(n,j))
    case Def(s@Reflect(MatrixGetRow(m,i), u, es)) if context.contains(s) => Some(m(i,n))
    case Def(s@Reflect(MatrixGetCol(m,j), u, es)) if context.contains(s) => Some(m(n,j))
    case _ => None
  }
  
  override def vectorview_apply[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    vectorview_optimize_apply(x.asInstanceOf[Exp[DeliteCollection[A]]],n) getOrElse super.vectorview_apply(x,n)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    vectorview_optimize_apply(x,n) getOrElse super.dc_apply(x,n)
  }
}

trait BaseGenVectorViewOps extends GenericFatCodegen {
  val IR: VectorViewOpsExp
  import IR._
  
  // override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
  //     case VectorViewApply(a, i) => Some((a,i))
  //     case _ => super.unapplySimpleIndex(e)
  //   }  
}

trait ScalaGenVectorViewOps extends BaseGenVectorViewOps with ScalaGenFat {
  val IR: VectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v@VectorViewNew(x,start,stride,length,isRow) => emitValDef(sym, "new VectorView[" + remap(v.mA) + "](" + quote(x) + "," + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ")")
    case VectorViewApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case VectorViewUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case VectorViewLength(x)    => emitValDef(sym, quote(x) + ".length")
    case VectorViewIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenVectorViewOps extends BaseGenVectorViewOps with CudaGenFat {
  val IR: VectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    //TODO: Allow this to only kernels (not helper functions)
    case VectorViewNew(x,start,stride,length,isRow) => {
      if(!processingHelperFunc) stream.println(remap(sym.Type) + " " + quote(sym) + "(" + quote(x) + "," + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ");")
      else throw new GenerationFailedException("CudaGen: VectorViewNew cannot be used in helper functions.")
    }
    case VectorViewApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorViewUpdate(x,n,y) => stream.println(quote(x) + ".update(" + quote(n) + "," + quote(y) + ");\n")
    case VectorViewLength(x)    => emitValDef(sym, quote(x) + ".length")
    case VectorViewIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
    case _ => super.emitNode(sym, rhs)
  }
}

  
