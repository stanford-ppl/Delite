package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector, DenseVector, RangeVector, IndexVector, IndexVectorDense}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait IndexVectorDenseOps extends Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecDenseOps(x: Rep[IndexVectorDense]) = new IndexVecDenseOpsCls(x)
  implicit def varToIndexVecDenseOps(x: Var[IndexVectorDense]) = new IndexVecDenseOpsCls(readVar(x))
  implicit def indexVecDenseToInterface(lhs: Rep[IndexVectorDense]) = new IVInterface(new IndexVecDenseOpsCls(lhs))
  
  implicit def indexVecDenseBuilder = new VectorBuilder[Int,IndexVectorDense] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = IndexVector(length)
    def toIntf(x: Rep[IndexVectorDense]): Interface[IndexVector] = indexVecDenseToInterface(x)
  }  
  
  // would like to have multiple inheritance here and inherit dense vector ops, but a
  // Rep[IndexVectorDense] and Rep[DenseVector] have no relation
  
  // for now, just duplicating the relevant implementations from DenseVectorOps :(
  
  class IndexVecDenseOpsCls(val elem: Rep[IndexVectorDense]) extends IndexVecOpsCls {    
    type Self = IndexVectorDense    
    def wrap(x: Rep[IndexVectorDense]) = indexVecDenseToInterface(x)
          
    // VectorOps
    def length = indexvectordense_length(elem)
    def apply(n: Rep[Int]) = indexvectordense_apply(elem,n)
    
    def isRow = throw new UnsupportedOperationException("tbd") 
    def sort(implicit o: Ordering[Int]) = throw new UnsupportedOperationException("tbd")     
    def t = throw new UnsupportedOperationException("tbd") 
    def mt() = throw new UnsupportedOperationException("tbd")    
    def update(n: Rep[Int], y: Rep[Int]): Rep[Unit] = throw new UnsupportedOperationException("tbd")
    def +=(y: Rep[Int]) = throw new UnsupportedOperationException("tbd")
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[Int]]) = throw new UnsupportedOperationException("tbd")
    def insert(pos: Rep[Int], y: Rep[Int]) = throw new UnsupportedOperationException("tbd")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[Int]]) = throw new UnsupportedOperationException("tbd")
    def removeAll(pos: Rep[Int], len: Rep[Int]) = throw new UnsupportedOperationException("tbd")
    def trim() = throw new UnsupportedOperationException("tbd")
    def clear() = throw new UnsupportedOperationException("tbd")        
  }   
  
  def indexvectordense_length(x: Rep[IndexVectorDense]): Rep[Int]
  def indexvectordense_apply(x: Rep[IndexVectorDense], n: Rep[Int]): Rep[Int]
}

trait IndexVectorDenseOpsExp extends IndexVectorDenseOps with DeliteCollectionOpsExp with VariablesExp with BaseFatExp { this: OptiMLExp => 
  case class IndexVectorDenseLength(x: Exp[IndexVectorDense]) extends Def[Int]
  case class IndexVectorDenseApply(x: Exp[IndexVectorDense], n: Exp[Int]) extends Def[Int]
    
  def indexvectordense_length(x: Exp[IndexVectorDense]) = reflectPure(IndexVectorDenseLength(x))
  def indexvectordense_apply(x: Exp[IndexVectorDense], n: Exp[Int]) = reflectPure(IndexVectorDenseApply(x,n))
    
  /////////////////////
  // delite collection
  
  def isIndexDense[A](x: Exp[DeliteCollection[A]]) = x.Type.erasure == classOf[IndexVectorDense]  
  def asIndexDense[A](x: Exp[DeliteCollection[A]]) = x.asInstanceOf[Exp[IndexVectorDense]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]) = { 
    if (isIndexDense(x)) asIndexDense(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = {
    if (isIndexDense(x)) (asIndexDense(x).apply(n)).asInstanceOf[Exp[A]]
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) = {
    if (isIndexDense(x)) asIndexDense(x).update(n,y.asInstanceOf[Exp[Int]])
    else super.dc_update(x,n,y)        
  }  
}

trait IndexVectorDenseOpsExpOpt extends IndexVectorDenseOpsExp { this: OptiMLExp => 
  
  override def indexvectordense_length(x: Rep[IndexVectorDense]) = x match {
    case Def(IndexVectorDenseNew(l)) => l
    case Def(v@Reflect(IndexVectorDenseNew(l), u, es)) if context.contains(v) => l
    case Def(IndexVectorObjectFromVec(xs)) => xs.length
    case Def(v@Reflect(IndexVectorObjectFromVec(xs), u, es)) if context.contains(v) => xs.length
    case _ => super.indexvectordense_length(x)
  }


  override def indexvectordense_apply(x: Rep[IndexVectorDense], n: Rep[Int]) = x match {
    case Def(IndexVectorObjectFromVec(xs)) => xs(n)
    case Def(v@Reflect(IndexVectorObjectFromVec(xs), u, es)) if context.contains(v) => xs(n)    
    case _ => super.indexvectordense_apply(x,n)
  }  
}


trait ScalaGenIndexVectorDenseOps extends ScalaGenFat {
  val IR: IndexVectorDenseOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case IndexVectorDenseLength(x) => emitValDef(sym, quote(x) + ".length")
    case IndexVectorDenseApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
  
