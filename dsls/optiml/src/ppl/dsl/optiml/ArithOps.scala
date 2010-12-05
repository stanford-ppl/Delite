package ppl.dsl.optiml

import scala.virtualization.lms.common.Base

/* Type class for basic math, but less restrictive than Numeric.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Nov 30, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

/*
trait ArithOps[T] {
  def +=(a: T, b: T) : T
  def +(a: T, b: T) : T
  /*
  def -(a: T, b: T) : T
  def *(a: T, b: T) : T
  def /(a: T, b: T) : T
  def zero : T
  def unary_-(a: T) : T
  def abs(a: T) : T
  def exp(a: T) : T
  def >(a: T, b: T) : T
  def <(a: T, b: T) : T
  */
}
*/


trait ArithOpsInternal[Rep[X],T] {
  def +=(a: Rep[T], b: Rep[T]) : Rep[T]
  def +(a: Rep[T], b: Rep[T]) : Rep[T]
  /*
  def -(a: T, b: T) : T
  def *(a: T, b: T) : T
  def /(a: T, b: T) : T
  def zero : T
  def unary_-(a: T) : T
  def abs(a: T) : T
  def exp(a: T) : T
  def >(a: T, b: T) : T
  def <(a: T, b: T) : T
  */
}



/*
trait ArithOps[T] {
  //type R[X]
  val IR: Base
  import IR._

  def +=(a: Rep[T], b: Rep[T]) : Rep[T]
  def +(a: Rep[T], b: Rep[T]) : Rep[T]
  /*
  def -(a: T, b: T) : T
  def *(a: T, b: T) : T
  def /(a: T, b: T) : T
  def zero : T
  def unary_-(a: T) : T
  def abs(a: T) : T
  def exp(a: T) : T
  def >(a: T, b: T) : T
  def <(a: T, b: T) : T
  */
}
*/