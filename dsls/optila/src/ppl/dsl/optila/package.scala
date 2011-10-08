package ppl.dsl

import ppl.delite.framework.datastruct.scala.DeliteCollection

package object optila {
  
  /**
   * OptiLA compiler types 
   */  
   
  trait Vector[T] extends DeliteCollection[T]
  trait DenseVector[T] extends Vector[T]
  trait SparseVector[T] extends Vector[T]
  trait RowVector[T] extends DenseVector[T]
  trait ColVector[T] extends DenseVector[T]
  trait ZeroVector[T] extends DenseVector[T]
  trait EmptyVector[T] extends DenseVector[T] 
  trait RangeVector extends DenseVector[Int]
  trait VectorView[T] extends DenseVector[T]
  trait MatrixRow[T] extends VectorView[T]
  trait MatrixCol[T] extends VectorView[T] 

  trait Matrix[T] extends DeliteCollection[T] 
  trait DenseMatrix[T] extends Matrix[T]
  trait SparseMatrix[T] extends Matrix[T]
  trait SymmetricMatrix[T] extends DenseMatrix[T]  
}