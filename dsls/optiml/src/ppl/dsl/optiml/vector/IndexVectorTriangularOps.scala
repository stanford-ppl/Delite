package ppl.dsl.optiml.vector

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.dsl.optiml._

/**
 * IndexVectorTriangular computes indices for a triangular matrix on the fly based
 * on the size of the triangular matrix.
 */
trait IndexVectorTriangularOps extends Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecTriangularOps(x: Rep[IndexVectorTriangular]) = new IndexVecTriangularOpsCls(x)
  implicit def varToIndexVecTriangularOps(x: Var[IndexVectorTriangular]) = new IndexVecTriangularOpsCls(readVar(x))
  implicit def indexVecTriangularToInterface(lhs: Rep[IndexVectorTriangular]) = new IV2Interface(new IndexVecTriangularOpsCls(lhs))
  
  object IndexVectorTriangular {
    def apply(n: Rep[Int], includeDiagonal: Rep[Boolean] = unit(true), upper: Rep[Boolean] = unit(true)) = indexvector_tri_new(n,includeDiagonal,upper)
  }
  
  class IndexVecTriangularOpsCls(val elem: Rep[IndexVectorTriangular]) extends IndexVec2OpsCls {
    type Self = IndexVectorTriangular
    type VA = DenseVector[(Int,Int)]
    def wrap(x: Rep[IndexVectorTriangular]) = indexVecTriangularToInterface(x)
    def vaToOps(x: Rep[VA]) = repToDenseVecOps(x)
    def vaToIntf(x: Rep[VA]) = denseVecToInterface(x)
    def vaBuilder(implicit ctx: SourceContext) = denseVectorBuilder
    def mVA = manifest[VA]
    
    // VectorOps
    def length(implicit ctx: SourceContext) = indexvector_tri_length(x)
    def isRow(implicit ctx: SourceContext) = unit(true)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = indexvector_tri_apply(x,n)
    def sort(implicit o: Ordering[(Int,Int)], ctx: SourceContext) = x.Clone    
    
    def t(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be transposed") // TODO    
    def mt()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")    
    def update(n: Rep[Int], y: Rep[(Int,Int)])(implicit ctx: SourceContext): Rep[Unit] = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[(Int,Int)]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
    def insert(pos: Rep[Int], y: Rep[(Int,Int)])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[(Int,Int)]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
    def trim()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
    def clear()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("TriangularVectors cannot be updated")
  } 
  
  def indexvector_tri_length(x: Rep[IndexVectorTriangular])(implicit ctx: SourceContext): Rep[Int]
  def indexvector_tri_apply(x: Rep[IndexVectorTriangular], n: Rep[Int])(implicit ctx: SourceContext): (Rep[Int],Rep[Int])
  def indexvector_tri_new(n: Rep[Int], diag: Rep[Boolean], upper: Rep[Boolean])(implicit ctx: SourceContext): Rep[IndexVectorTriangular]
}

trait IndexVectorTriangularOpsExp extends IndexVectorTriangularOps with DeliteCollectionOpsExp { this: OptiMLExp =>
  case class IndexVectorTriangularNew(n: Exp[Int], d: Exp[Boolean], upper: Exp[Boolean]) extends Def[IndexVectorTriangular]
      
  def triangle_size(x: Rep[Int]) = x*(x+1)/2 // arithmetic sequence sum
  
  def triangle_length(x: Rep[Int], d: Rep[Boolean]) = {
    val end = if (d) x else x - 1
    triangle_size(end)
  } 

  // http://stackoverflow.com/a/244550  
  // O(1), but with non-trivial overhead. We should compare against the explicitly stored version.
  def utriangle_apply(sz: Rep[Int], d: Rep[Boolean], n: Rep[Int], up: Rep[Boolean]) = {
    if (up == unit(false)) fatal(unit("lower triangular matrix is not implemented yet"))
    
    val m = if (d) sz else sz-1
    val off = if (d) unit(0) else unit(1)
    val t = triangle_size(m)-1-n
    val k = floor((sqrt(8*t+1)-unit(1))/unit(2)).AsInstanceOf[Int]
    val row = m-1-k
    (row, (n+triangle_size(row))%m+off)
  }
  
  def indexvector_tri_length(x: Rep[IndexVectorTriangular])(implicit ctx: SourceContext) = x match {
    case Def(IndexVectorTriangularNew(sz,d,up)) => triangle_length(sz,d)
    case Def(v@Reflect(IndexVectorTriangularNew(sz,d,up), u, es)) /*if context.contains(v)*/ => triangle_length(sz,d)
  }
  
  def indexvector_tri_apply(x: Rep[IndexVectorTriangular], n: Rep[Int])(implicit ctx: SourceContext) = x match {
    case Def(IndexVectorTriangularNew(sz,d,up)) => utriangle_apply(sz,d,n,up) 
    case Def(v@Reflect(IndexVectorTriangularNew(sz,d,up), u, es)) /*if context.contains(v)*/ => utriangle_apply(sz,d,n,up) 
  }  
  
  def indexvector_tri_new(n: Rep[Int], diag: Rep[Boolean], up: Rep[Boolean])(implicit ctx: SourceContext) = reflectPure(IndexVectorTriangularNew(n,diag,up))
    
  /////////////////////
  // delite collection
  
  def isIndexTriangular[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.tp.erasure == classOf[IndexVectorTriangular]  
  def asIndexTriangular[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[IndexVectorTriangular]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isIndexTriangular(x)) asIndexTriangular(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isIndexTriangular(x)) (asIndexTriangular(x).apply(n)).asInstanceOf[Exp[A]]
    else super.dc_apply(x,n)    
  }  
}
