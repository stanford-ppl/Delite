package ppl.dsl.optiml.datastruct.scala

// TODO: putting everything in one file was convenient at first, but now that this is growing larger
// it should be refactored into a more logical organization.

/**
 * Delite
 */

/**
 * @tparam CR  A subtype of DeliteCollection[B]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
trait DeliteOpMap[@specialized A, @specialized B, CR] {
  def in: DeliteCollection[A]
  def alloc: CR
  def map(a: A): B
}

/**
 * @tparam CR  A subtype of DeliteCollection[R]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
trait DeliteOpZipWith[@specialized A, @specialized B, @specialized R, CR] {
  def inA: DeliteCollection[A]
  def inB: DeliteCollection[B]
  def alloc: CR
  def zip(a: A, b: B): R
}

trait DeliteOpReduce[@specialized R] {
  def in: DeliteCollection[R]
  def reduce(r1: R, r2: R): R
}

trait DeliteOpMapReduce[@specialized A, @specialized R] {
  def in: DeliteCollection[A]
  def map(elem: A): R
  def reduce(r1: R, r2: R): R

  /**
   * default implementation of map-reduce is simply to compose the map and reduce functions
   * A subclass can override to fuse the implementations
   */
  def mapreduce(acc: R, elem: A): R = reduce(acc, map(elem))
}

trait DeliteOpZipWithReduce[@specialized A, @specialized B, @specialized R] {
  def inA: DeliteCollection[A]
  def inB: DeliteCollection[B]
  def zip(a: A, b: B): R
  def reduce(r1: R, r2: R): R

  /**
   * default implementation of zip-reduce is simply to compose the zip and reduce functions
   * A subclass can override to fuse the implementations
   */
  def zipreduce(acc: R, a: A, b: B): R = reduce(acc, zip(a,b))
}

trait DeliteOpForeach[@specialized A] {
  def in: DeliteCollection[A]
  def foreach(elem: A): Unit
  def sync(idx: Int): List[_]
}

trait DeliteCollection[@specialized T] {
  def size: Int
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T)
}

/**
 * Vector
 */

trait Vector[@specialized T] extends ppl.delite.framework.DeliteCollection[T] {
  // methods required on real underlying data structure impl
  // we need these for:
  //   1) accessors to data fields
  //   2) setters to data fields (alternatively, methods that can mutate data fields)
  //   3) methods that the runtime expects
  def length : Int
  def isRow : Boolean
  def apply(n: Int) : T
  def update(index: Int, x: T)
  def data: Array[T]

  def mtrans: Vector[T]
  def sort(implicit o: Ordering[T]): Vector[T] // because we use the underlying data field to sort
  def copyFrom(pos: Int, xs: Vector[T])
  def insert(pos: Int, x: T)
  def insertAll(pos: Int, xs: Vector[T])
  def removeAll(pos: Int, len: Int)
  def trim
  def cloneL: Vector[T]

  // DeliteCollection
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def size = length
}

trait NilVector[@specialized T] extends Vector[T] {
  def length : Int = 0
  def apply(i: Int) = throw new UnsupportedOperationException()
  def isRow : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: T) = throw new UnsupportedOperationException()
  def data = throw new UnsupportedOperationException()

  def mtrans = throw new UnsupportedOperationException()
  def sort(implicit o: Ordering[T]) = throw new UnsupportedOperationException()
  def insert(pos: Int, x: T) = throw new UnsupportedOperationException()
  def insertAll(pos: Int, xs: Vector[T]) = throw new UnsupportedOperationException()
  def copyFrom(pos: Int, xs: Vector[T]) = throw new UnsupportedOperationException()
  def removeAll(pos: Int, len: Int) = throw new UnsupportedOperationException()
  def trim = throw new UnsupportedOperationException()
  def cloneL = throw new UnsupportedOperationException()
}

trait VectorView[@specialized T] extends Vector[T]

trait RangeVector extends Vector[Int]

trait IndexVector extends Vector[Int]

/**
 * Matrix
 */
trait Matrix[@specialized T] extends ppl.delite.framework.DeliteCollection[T] {
  // fields required on real underlying data structure impl
  def numRows: Int
  def numCols: Int
  def size: Int
  def data: Array[T]

  def apply(i: Int) : VectorView[T]
  def apply(i: Int, j: Int): T
  def update(row: Int, col: Int, x: T)
  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
  def insertRow(pos: Int, x: Vector[T])
  def insertAllRows(pos: Int, xs: Matrix[T])
  def insertCol(pos: Int, x: Vector[T])
  def insertAllCols(pos: Int, xs: Matrix[T])
  def removeRows(pos: Int, len: Int)
  def removeCols(pos: Int, len: Int)
  def cloneL: Matrix[T]

  // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit

}

/**
 * TrainingSet
 */

trait Labels[@specialized L] extends Vector[L] {
  def numLabels = length
}

trait TrainingSet[@specialized T,L] extends Matrix[T] {
  def numSamples = numRows
  def numFeatures = numCols
  def labels: Labels[L]

  def transposed: TrainingSet[T,L]
  override def update(row: Int, col: Int, x: T) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertRow(pos: Int, x: Vector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertAllRows(pos: Int, xs: Matrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertCol(pos: Int, x: Vector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertAllCols(pos: Int, xs: Matrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def removeRows(pos: Int, len: Int) = throw new UnsupportedOperationException("Training sets are immutable")
  override def removeCols(pos: Int, len: Int) = throw new UnsupportedOperationException("Training sets are immutable")
}

/**
 * Ref
 */

case class Ref[@specialized T](v: T) {
  private var _v = v

  def get = _v
  def set(v: T) = _v = v
}
