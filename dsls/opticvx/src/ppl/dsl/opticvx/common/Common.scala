package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

class IRValidationException extends Exception

trait IntLike[T] {
  def add(x: T, y: T): T
  def neg(x: T): T
  def multiply(x: T, y: T): T
  def divide(x: T, r: Int): T

  implicit def int2T(x: Int): T

  class IntLikeTHack(val t: T) {
    def unary_-(): T = neg(t)
    def +(u: T) = add(t, u)
    def -(u: T) = add(t, neg(u))
    def *(u: T) = multiply(t, u)
    def /(r: Int) = divide(t, r)
  }
  implicit def intlikethackimpl(t: T) = new IntLikeTHack(t)
}

object IntLikeInt extends IntLike[Int] {
  def add(x: Int, y: Int): Int = x + y
  def neg(x: Int): Int = -x
  def multiply(x: Int, y: Int): Int = x * y
  def divide(x: Int, r: Int): Int = {
    if((x % r) != 0) throw new IRValidationException()
    x / r
  }

  implicit def int2T(x: Int): Int = x
}
