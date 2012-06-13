package ppl.dsl.optila.vector

import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._

import ppl.dsl.optila._

trait RangeVectorOps extends Base with OverloadHack { this: OptiLA =>

  implicit def repToRangeVecOps(x: Rep[RangeVector]) = new RangeVecOpsCls(x)
  implicit def varToRangeVecOps(x: Var[RangeVector]) = new RangeVecOpsCls(readVar(x))
  implicit def rangeToInterface(lhs: Rep[RangeVector]) = new VInterface(new RangeVecOpsCls(lhs))
  
  implicit def rangeVectorBuilder = new VectorBuilder[Int,RangeVector] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = (unit(0)::length)
    def toIntf(x: Rep[RangeVector]): Interface[Vector[Int]] = rangeToInterface(x)
  }  
  
  class RangeVecOpsCls(val elem: Rep[RangeVector]) extends VecOpsCls[Int] {    
    type V[X] = DenseVector[X]
    type M[X] = DenseMatrix[X]
    type I[X] = DenseMatrix[X]
    type VA = DenseVector[Int]
    type Self = RangeVector 
    
    def mA = manifest[Int]    
    def mVA = manifest[VA]    
    def mV[B:Manifest] = manifest[DenseVector[B]]
    def mM[B:Manifest] = manifest[DenseMatrix[B]]    
    def wrap(x: Rep[RangeVector]) = rangeToInterface(x)    
    def vaToOps(x: Rep[VA]) = vecToOps[Int](x)
    def vaToIntf(x: Rep[VA]) = vecToIntf[Int](x)
    def vaBuilder(implicit ctx: SourceContext) = vecBuilder[Int]      
    def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]): Interface[Matrix[B]] = denseMatToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = denseMatrixBuilder[B]

    // VectorOps
    def length(implicit ctx: SourceContext) = rangevector_length(elem)
    def isRow(implicit ctx: SourceContext) = rangevector_isrow(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = rangevector_apply(elem,n)
    def sort(implicit o: Ordering[Int], ctx: SourceContext) = elem.Clone    
    
    // should forward to a RangeVectorOpsExp implementation which throws an OptiLA compiler error instead of using exceptions
    def t(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be transposed") // TODO    
    def mt()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")    
    def update(n: Rep[Int], y: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[Int]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def insert(pos: Rep[Int], y: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[Int]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def trim()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def clear()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("RangeVectors cannot be updated")        
  } 
  
  def rangevector_length(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int]
  def rangevector_isrow(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Boolean]
  def rangevector_apply(x: Rep[RangeVector], n: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  // def rangevector_times_matrix(x: Rep[RangeVector], y: Rep[Matrix[Int]]): Rep[DenseVector[Int]]
  // def rangevector_flatmap[B:Manifest](x: Rep[RangeVector], f: Rep[A] => Rep[DenseVector[B]]): Rep[DenseVector[B]]
}

trait RangeVectorOpsExp extends RangeVectorOps with DeliteCollectionOpsExp { this: OptiLAExp =>
  
  def rangevector_length(x: Rep[RangeVector])(implicit ctx: SourceContext) = x match {
    case Def(VectorObjectRange(start,end,stride,r)) => (end-start + stride - unit(1)) / stride
    case Def(v@Reflect(VectorObjectRange(start,end,stride,r), u, es)) /*if context.contains(v)*/ => (end-start + stride - unit(1)) / stride
  }
  
  def rangevector_isrow(x: Exp[RangeVector])(implicit ctx: SourceContext) = x match {
    case Def(VectorObjectRange(s,e,d,r)) => r
    case Def(v@Reflect(VectorObjectRange(s,e,d,r), u, es)) /*if context.contains(v)*/ => r
  }
  
  def rangevector_apply(x: Exp[RangeVector], n: Exp[Int])(implicit ctx: SourceContext) = rangevector_optimize_apply(x,n).get
  
  // and this one also helps in the example:
  def rangevector_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Option[Exp[A]] = x match {
    case Def(VectorObjectRange(s,e,d,r)) => Some((s + n*d).asInstanceOf[Exp[A]])
    case Def(v@Reflect(VectorObjectRange(s,e,d,r), u, es)) /*if context.contains(v)*/ => Some((s + n*d).asInstanceOf[Exp[A]])
    case _ => None
  }
  
  /////////////////////
  // delite collection
    
  def isRange[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[RangeVector])  
  def asRange[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[RangeVector]]
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isRange(x)) asRange(x).length
    else super.dc_size(x)
  }
    
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isRange(x)) rangevector_optimize_apply(asRange(x),n).getOrElse(super.dc_apply(x,n)).asInstanceOf[Exp[A]]
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isRange(x)) err("dc_update should not be called on RangeVector")
    else super.dc_update(x,n,y)        
  }
  
  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isRange(x)) err("dc_append should not be called on RangeVector")
    else super.dc_append(x,i,y)        
  }    
  
}
  
