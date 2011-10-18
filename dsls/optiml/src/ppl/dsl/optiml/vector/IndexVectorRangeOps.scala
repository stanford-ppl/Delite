package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector, DenseVector, RangeVector, IndexVector, IndexVectorRange}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait IndexVectorRangeOps extends DSLType with Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecRangeOps(x: Rep[IndexVectorRange]) = new IndexVecRangeOpsCls(x)
  implicit def varToIndexVecRangeOps(x: Var[IndexVectorRange]) = new IndexVecRangeOpsCls(readVar(x))
  implicit def indexVecRangeToInterface(lhs: Rep[IndexVectorRange]) = new IVInterface(new IndexVecRangeOpsCls(lhs))
  
  // hack to allow index range vector builders to be found without explicit type parameters
  //implicit def indexRangeVecBuilderGeneric[A <% Int]: VectorBuilder[A,IndexVectorRange] = indexRangeVecBuilder.asInstanceOf[VectorBuilder[A,IndexVectorRange]]
  // manifest[A] match {
  //     case Manifest.Int => indexRangeVecBuilder
  //   }  
  implicit def indexRangeVecBuilder = new VectorBuilder[Int,IndexVectorRange] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = (0::length)
    def toIntf(x: Rep[IndexVectorRange]): Interface[IndexVector] = indexVecRangeToInterface(x)
  }  
  
  class IndexVecRangeOpsCls(val elem: Rep[IndexVectorRange]) extends IndexVecOpsCls {
    type Self = IndexVectorRange
    def selfToIntf(x: Rep[IndexVectorRange]) = indexVecRangeToInterface(x)
          
    // VectorOps
    def length = indexvectorrange_length(x)
    def isRow = unit(true)
    def apply(n: Rep[Int]) = n
    def sort(implicit o: Ordering[Int]) = x.cloneL    
    
    def t = throw new UnsupportedOperationException("RangeVectors cannot be transposed") // TODO    
    def mt() = throw new UnsupportedOperationException("RangeVectors cannot be updated")    
    def update(n: Rep[Int], y: Rep[Int]): Rep[Unit] = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def +=(y: Rep[Int]) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[Int]]) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def insert(pos: Rep[Int], y: Rep[Int]) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[Int]]) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int]) = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def trim() = throw new UnsupportedOperationException("RangeVectors cannot be updated")
    def clear() = throw new UnsupportedOperationException("RangeVectors cannot be updated")        
  } 
  
  def indexvectorrange_length(x: Rep[IndexVectorRange]): Rep[Int]
  // def indexvectorrange_times_matrix(x: Rep[IndexVectorRange], y: Rep[Matrix[Int]]): Rep[DenseVector[Int]]
  // def indexvectorrange_flatmap[B:Manifest](x: Rep[IndexVectorRange], f: Rep[A] => Rep[DenseVector[B]]): Rep[DenseVector[B]]
}

trait IndexVectorRangeOpsExp extends IndexVectorRangeOps { this: OptiMLExp =>
  
  def indexvectorrange_length(x: Rep[IndexVectorRange]) = x match {
    case Def(IndexVectorRangeNew(s, e)) => e - s
  }
  
}
  
