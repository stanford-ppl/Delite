package ppl.dsl.optiml.datastruct.scala

/**
 * Delite
 */

/**
 * @tparam CR  A subtype of DeliteCollection[B]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
trait DeliteOpMap[@specialized A, @specialized B, CR] {
  def in: DeliteCollection[A]
  def out: CR
  def map(a: A): B
}

/**
 * @tparam CR  A subtype of DeliteCollection[R]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
trait DeliteOpZipWith[@specialized A, @specialized B, @specialized R, CR] {
  def inA: DeliteCollection[A]
  def inB: DeliteCollection[B]
  def out: CR
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
  // fields required on real underlying data structure impl
  def length : Int
  def is_row : Boolean
  def apply(n: Int) : T
  def update(index: Int, x: T)

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)

  // DeliteCollection
  def size = length
}

trait NilVector[@specialized T] extends Vector[T]

trait VectorView[@specialized T] extends Vector[T]

/**
 * Matrix
 */
trait Matrix[@specialized T] extends ppl.delite.framework.DeliteCollection[T] {
  // fields required on real underlying data structure impl
  def numRows: Int
  def numCols: Int
  def size: Int

  def apply(i: Int) : VectorView[T]
  def apply(i: Int, j: Int) : T
  def update(row: Int, col: Int, x: T)
  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : VectorView[T]
  def insertRow(pos: Int, x: Vector[T]): Matrix[T]
}


/**
 * Ref
 */

case class Ref[@specialized T](v: T) {
  private var _v = v

  def get = _v
  def set(v: T) = _v = v
}
