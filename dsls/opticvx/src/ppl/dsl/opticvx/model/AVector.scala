package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait AVectorLike[T <: HasArity[T]] {
  def size(arg: T): IRPoly
  def zero(size: IRPoly): T
  def add(arg1: T, arg2: T): T
  def addfor(len: IRPoly, arg:T): T
  def neg(arg: T): T
  def scaleinput(arg: T, scale: IRPoly): T
  def scaleconstant(arg: T, scale: Double): T
  def cat(arg1: T, arg2:T): T
  def catfor(len: IRPoly, arg: T): T
  def slice(arg: T, at: IRPoly, size: IRPoly): T

  class THackImpl(val t: T) {
    def +(u: T) = add(t, u)
    def -(u: T) = add(t, neg(u))
    def unary_-() = neg(t)
    def ++(u: T) = cat(t, u)
    def apply(at: IRPoly, size: IRPoly) = slice(t, at, size)
  }

  implicit def t2thackimpl(t: T) = new THackImpl(t)
}
