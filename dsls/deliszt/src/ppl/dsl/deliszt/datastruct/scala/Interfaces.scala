package ppl.dsl.deliszt.datastruct.scala

// TODO: putting everything in one file was convenient at first, but now that this is growing larger
// it should be refactored into a more logical organization.

//TR: Note that scalac's specialize phase took 15s (!) on this file (on my MacBook).
// No wonder since DeliteOpZipWith and DeliteOpZipWithReduce each had 1000 specialized variants.
// Adding @specialized(Boolean, Int, Long, Float, Double) reduced the time to 5s.
// Note also that DeliteOpMultiLoop does not have that problem because it always
// uses a custom activation record class for each use.

// I changed some of the traits to abstract classes, that might yield a slight speed gain
// at runtime (invokevirtual vs invokeinterface)

/**
 * Delite
 */

abstract class DeliteOpMultiLoop[A] {
  def size: Int
  def alloc: A
  def split(rhs: A): A
  def process(__act: A, idx: Int): Unit
  def combine(__act: A, rhs: A): Unit
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
  def size: Int
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T)
}

/**
 * Vector
 */

// TODO: vector and matrix should not expose "data" fields. This needs to be refactored.















/**
 * Matrix
 */


/**
 * TrainingSet
 */





/**
 * Image
 */





/**
 * Graph
 */

// no covariance here, since Graph is mutable.


// this is really not pretty. this class hierarchy needs to be thought about more than i've had a chance to.




// this is really not pretty. this class hierarchy needs to be thought about more than i've had a chance to.







// no covariance here, since Graph is mutable.



/**
 * Bidirectional graph
 */









/**
 * Ref
 */

case class Ref[@specialized(Boolean, Int, Long, Float, Double) T](v: T) {
  private[this] var _v = v

  def get = _v
  def set(v: T) = _v = v
}
