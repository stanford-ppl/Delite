package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector, DenseVector, RangeVector, IndexVector, IndexVectorDense}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait IndexVectorDenseOps extends DSLType with Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecDenseOps(x: Rep[IndexVectorDense]) = new IndexVecDenseOpsCls(x)
  implicit def varToIndexVecDenseOps(x: Var[IndexVectorDense]) = new IndexVecDenseOpsCls(readVar(x))
  implicit def indexVecDenseToInterface(lhs: Rep[IndexVectorDense]) = new IVInterface(new IndexVecDenseOpsCls(lhs))
  
  def indexVecDenseBuilder = new VectorBuilder[Int,IndexVectorDense] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = IndexVector(length)
    def toIntf(x: Rep[IndexVectorDense]): Interface[IndexVector] = indexVecDenseToInterface(x)
  }  
  
  // would like to have multiple inheritance here and inherit dense vector ops, but a
  // Rep[IndexVectorDense] and Rep[DenseVector] have no relation
  
  // for now, just duplicating the relevant implementations from DenseVectorOps :(
  
  class IndexVecDenseOpsCls(val elem: Rep[IndexVectorDense]) extends IndexVecOpsCls {    
    type Self = IndexVectorDense    
    def selfToIntf(x: Rep[IndexVectorDense]) = indexVecDenseToInterface(x)
          
    // VectorOps
    def length = indexvectordense_length(elem)
    
    def isRow = throw new UnsupportedOperationException("tbd") 
    def apply(n: Rep[Int]) = throw new UnsupportedOperationException("tbd") 
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
}

trait IndexVectorDenseOpsExp extends IndexVectorDenseOps with VariablesExp with BaseFatExp { this: OptiMLExp => 
  case class IndexVectorDenseLength(x: Exp[IndexVectorDense]) extends Def[Int]
  
  def indexvectordense_length(x: Exp[IndexVectorDense]) = reflectPure(IndexVectorDenseLength(x))
}

trait ScalaGenIndexVectorDenseOps extends ScalaGenFat {
  val IR: IndexVectorDenseOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case IndexVectorDenseLength(x) => emitValDef(sym, quote(x) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}
  
