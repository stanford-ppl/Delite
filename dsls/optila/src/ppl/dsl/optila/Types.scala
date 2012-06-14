package ppl.dsl.optila

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * OptiLA compiler types
 */

// ideally, can have subtyping at the front-end and not at the back-end
// subtypes should be able to instantiate a superset of the supertype struct,
// and subtype operations can operate on any of those fields. supertype operations
// are still dispatched statically to operate on the subset of fields it knows about.

// the issue is that if we lose the static type, the subtype fields of the struct are
// essentially dead, since they can no longer be dispatched to..

// subtyping is also required for interface covariance; Interface[Vector[T]] <:< Interface[DeliteCollection[T]]

/**
 * Vector 
 */
 
// involve representation & require their own ops to implement vector abstract methods
abstract class Vector[T] extends DeliteCollection[T]
trait DenseVector[T] extends Vector[T]
trait SparseVector[T] extends Vector[T]
//trait ZeroVector[T] extends DenseVector[T]
//trait EmptyVector[T] extends DenseVector[T]

// Range and View should never dispatch to Dense ops, because the Dense implementation of abstract vector methods is incorrect for them
trait RangeVector extends Vector[Int] with RowVector[Int]
trait DenseVectorView[T] extends Vector[T] 
trait SparseVectorView[T] extends Vector[T]

// these do not add any functionality, but are used for type-checking
// the mix-ins define their possible static dispatch receivers
// the issue if we still use subtyping to do some of the dispatch is that
// return types are not preserved; MatrixRow + 5 => DenseVectorView[T]
trait RowVector[T]
trait ColVector[T]
trait DenseRowVector[T] extends DenseVector[T] with RowVector[T]
trait DenseColVector[T] extends DenseVector[T] with ColVector[T]
trait SparseRowVector[T] extends SparseVector[T] with RowVector[T]
trait SparseColVector[T] extends SparseVector[T] with ColVector[T]

/**
 * Matrix 
 */
 
abstract class Matrix[T] extends DeliteCollection[T]
trait MatrixBuildable[T] extends DeliteCollection[T]
trait DenseMatrix[T] extends Matrix[T] with MatrixBuildable[T]
trait Image[T] extends DenseMatrix[T]
trait SparseMatrix[T] extends Matrix[T]                   // used for sparse matrix operations
trait SparseMatrixBuildable[T] extends MatrixBuildable[T] // used for sparse matrix construction
//trait SymmetricMatrix[T] extends DenseMatrix[T]