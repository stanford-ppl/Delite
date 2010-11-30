package ppl.dsl.optiml

/* Type class for basic math, but less restrictive than Numeric.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Nov 30, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait ArithOps[T] {
  def +=(a: T, b: T) : Unit
  /*
  def +(a: T, b: T) : T
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