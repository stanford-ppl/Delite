package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector, DenseVector, RangeVector, IndexVector, IndexVectorDense}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter

trait IndexVectorDenseOps extends Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecDenseOps(x: Rep[IndexVectorDense]) = new IndexVecDenseOpsCls(x)
  implicit def varToIndexVecDenseOps(x: Var[IndexVectorDense]) = new IndexVecDenseOpsCls(readVar(x))
  implicit def indexVecDenseToInterface(lhs: Rep[IndexVectorDense]) = new IVInterface(new IndexVecDenseOpsCls(lhs))
  
  // implicit def indexVecDenseBuilder = new VectorBuilder[Int,IndexVectorDense] {
  //   def alloc(length: Rep[Int], isRow: Rep[Boolean]) = IndexVector(length)
  //   def toIntf(x: Rep[IndexVectorDense]): Interface[IndexVector] = indexVecDenseToInterface(x)
  // }  
  
  // would like to have multiple inheritance here and inherit dense vector ops, but a
  // Rep[IndexVectorDense] and Rep[DenseVector] have no relation
  
  // for now, just duplicating the relevant implementations from DenseVectorOps :(
  
  class IndexVecDenseOpsCls(val elem: Rep[IndexVectorDense]) extends IndexVecOpsCls {    
    type Self = IndexVectorDense    
    def wrap(x: Rep[IndexVectorDense]) = indexVecDenseToInterface(x)
          
    // VectorOps
    def length(implicit ctx: SourceContext) = indexvectordense_length(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = indexvectordense_apply(elem,n)
    
    def isRow(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd") 
    def sort(implicit o: Ordering[Int], ctx: SourceContext) = throw new UnsupportedOperationException("tbd")     
    def t(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd") 
    def mt()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")    
    def update(n: Rep[Int], y: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = throw new UnsupportedOperationException("tbd")
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[Int]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def insert(pos: Rep[Int], y: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[Int]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def trim()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def clear()(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")        
  }   
  
  def indexvectordense_length(x: Rep[IndexVectorDense])(implicit ctx: SourceContext): Rep[Int]
  def indexvectordense_apply(x: Rep[IndexVectorDense], n: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
}

trait IndexVectorDenseOpsExp extends IndexVectorDenseOps with DeliteCollectionOpsExp with VariablesExp with BaseFatExp { this: OptiMLExp => 
  case class IndexVectorDenseLength(x: Exp[IndexVectorDense]) extends Def[Int]
  case class IndexVectorDenseApply(x: Exp[IndexVectorDense], n: Exp[Int]) extends Def[Int]
    
  def indexvectordense_length(x: Exp[IndexVectorDense])(implicit ctx: SourceContext) = reflectPure(IndexVectorDenseLength(x))
  def indexvectordense_apply(x: Exp[IndexVectorDense], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(IndexVectorDenseApply(x,n))
    
  /////////////////////
  // delite collection
  
  def isIndexDense[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.Type.erasure == classOf[IndexVectorDense]  
  def asIndexDense[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[IndexVectorDense]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isIndexDense(x)) asIndexDense(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isIndexDense(x)) (asIndexDense(x).apply(n)).asInstanceOf[Exp[A]]
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isIndexDense(x)) asIndexDense(x).update(n,y.asInstanceOf[Exp[Int]])
    else super.dc_update(x,n,y)        
  }  
}

trait IndexVectorDenseOpsExpOpt extends IndexVectorDenseOpsExp { this: OptiMLExp => 
  
  override def indexvectordense_length(x: Rep[IndexVectorDense])(implicit ctx: SourceContext) = x match {
    case Def(IndexVectorDenseNew(l)) => l
    case Def(v@Reflect(IndexVectorDenseNew(l), u, es)) if context.contains(v) => l
    case Def(IndexVectorObjectFromVec(xs)) => xs.length
    case Def(v@Reflect(IndexVectorObjectFromVec(xs), u, es)) if context.contains(v) => xs.length
    case _ => super.indexvectordense_length(x)
  }


  override def indexvectordense_apply(x: Rep[IndexVectorDense], n: Rep[Int])(implicit ctx: SourceContext) = x match {
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
  
