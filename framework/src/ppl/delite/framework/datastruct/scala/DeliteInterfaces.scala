package ppl.delite.framework.datastruct.scala


/**
 * Delite
 */

abstract class DeliteOpMultiLoop[A] {
  def size: Int
  def alloc: A
  def processRange(__act: A, start: Int, end: Int): A //init+process
  def init(__act: A, idx: Int): A
  def process(__act: A, idx: Int): Unit
  def combine(__act: A, rhs: A): Unit
  def postCombine(__act: A, rhs: A): Unit
  def postProcInit(__act: A): Unit
  def postProcess(__act: A): Unit
  def finalize(__act: A): Unit
}

/**
 * @tparam CR  A subtype of DeliteCollection[B]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
abstract class DeliteOpMap[@specialized(Boolean, Int, Long, Float, Double) A, 
                  @specialized(Boolean, Int, Long, Float, Double) B, CR] {
  def in: DeliteCollection[A]
  def alloc: CR
  def map(a: A): B
}

/**
 * @tparam CR  A subtype of DeliteCollection[R]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
abstract class DeliteOpZipWith[@specialized(Boolean, Int, Long, Float, Double) A,
                      @specialized(Boolean, Int, Long, Float, Double) B, 
                      @specialized(Boolean, Int, Long, Float, Double) R, CR] {
  def inA: DeliteCollection[A]
  def inB: DeliteCollection[B]
  def alloc: CR
  def zip(a: A, b: B): R
}

abstract class DeliteOpReduce[@specialized(Boolean, Int, Long, Float, Double) R] {
  def in: DeliteCollection[R]
  def reduce(r1: R, r2: R): R
}

abstract class DeliteOpMapReduce[@specialized(Boolean, Int, Long, Float, Double) A, 
                        @specialized(Boolean, Int, Long, Float, Double) R] {
  def in: DeliteCollection[A]
  def map(elem: A): R
  def reduce(r1: R, r2: R): R

  /**
   * default implementation of map-reduce is simply to compose the map and reduce functions
   * A subclass can override to fuse the implementations
   */
  def mapreduce(acc: R, elem: A): R = reduce(acc, map(elem))
}

abstract class DeliteOpZipWithReduce[@specialized(Boolean, Int, Long, Float, Double) A, 
                            @specialized(Boolean, Int, Long, Float, Double) B, 
                            @specialized(Boolean, Int, Long, Float, Double) R] {
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

abstract class DeliteOpForeach[@specialized(Boolean, Int, Long, Float, Double) A] {
  def in: DeliteCollection[A]
  def foreach(elem: A): Unit
  def sync(idx: Int): List[Any]
}

trait DeliteCollection[@specialized(Boolean, Int, Long, Float, Double) T] {
  def dcSize: Int
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T)
}


/**
 * Ref
 */

class Ref[@specialized(Boolean, Int, Long, Float, Double) T](v: T) {
  private[this] var _v = v

  def get = _v
  def set(v: T) = _v = v
}
