package ppl.dsl.deliszt.capabilities

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


trait ArithInternal[Rep[X], T] {
  //def +=(a: Rep[T], b: Rep[T]): Rep[T]
  def +(a: Rep[T], b: Rep[T]): Rep[T]
  def -(a: Rep[T], b: Rep[T]): Rep[T]
  def *(a: Rep[T], b: Rep[T]): Rep[T]
  def /(a: Rep[T], b: Rep[T]): Rep[T]
  def abs(a: Rep[T]): Rep[T]
  def exp(a: Rep[T]): Rep[T]
  def unary_-(a: Rep[T]): Rep[T]
  def empty: Rep[T]
  def zero(a: Rep[T]): Rep[T]
}
