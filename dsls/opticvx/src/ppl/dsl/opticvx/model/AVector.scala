package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait AVectorLike[T <: HasArity[T]] {
  def size(arg: T): IRPoly
  def zero(size: IRPoly): T
  def one(size: IRPoly): T
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

object AVectorLikeAVector extends AVectorLike[AVector] {
  def size(arg: AVector): IRPoly = arg.size
  def zero(size: IRPoly): AVector = AVectorZero(size)
  def one(size: IRPoly): AVector = AVectorOne(size)
  def add(arg1: AVector, arg2: AVector): AVector = AVectorAdd(arg1, arg2)
  def addfor(len: IRPoly, arg: AVector): AVector = AVectorAddFor(len, arg)
  def neg(arg: AVector): AVector = AVectorNeg(arg)
  def scaleinput(arg: AVector, scale: IRPoly): AVector = AVectorScaleInput(arg, scale)
  def scaleconstant(arg: AVector, scale: Double): AVector = AVectorScaleConstant(arg, scale)
  def cat(arg1: AVector, arg2: AVector): AVector = AVectorCat(arg1, arg2)
  def catfor(len: IRPoly, arg: AVector): AVector = AVectorCatFor(len, arg)
  def slice(arg: AVector, at: IRPoly, size: IRPoly): AVector = AVectorSlice(arg, at, size)
}

object AVector {
  import AVectorLikeAVector._
  def input(at: IRPoly, len: IRPoly): AVector = {
    if(at.arity != len.arity) throw new IRValidationException()
    catfor(len, scaleinput(one(IRPoly.const(1, at.arity + 1)), at.promote + at.next))
  }
  def const(c: Double, arity: Int): AVector = {
    scaleconstant(one(IRPoly.const(1, arity)), c)
  }
}

trait AVector extends HasArity[AVector] {
  def size: IRPoly
  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V

  def +(u: AVector) = AVectorAdd(this, u)
  def -(u: AVector) = AVectorAdd(this, AVectorNeg(u))
  def unary_-() = AVectorNeg(this)
  def ++(u: AVector) = AVectorCat(this, u)
  def apply(at: IRPoly, size: IRPoly) = AVectorSlice(this, at, size)
}

case class AVectorZero(val size: IRPoly) extends AVector {
  val arity: Int = size.arity  
  def arityOp(op: ArityOp): AVector = AVectorZero(size.arityOp(op))
  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.zero(size)
}

case class AVectorOne(val size: IRPoly) extends AVector {
  val arity: Int = size.arity  
  def arityOp(op: ArityOp): AVector = AVectorZero(size.arityOp(op))
  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.one(size)
}

case class AVectorAdd(val arg1: AVector, val arg2: AVector) extends AVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size

  if(arg1.size != arg2.size) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorAdd(arg1.arityOp(op), arg2.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.add(arg1.translate, arg2.translate)
}

case class AVectorNeg(val arg: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size

  def arityOp(op: ArityOp): AVector = AVectorNeg(arg.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.neg(arg.translate)
}

case class AVectorScaleInput(val arg: AVector, val scale: IRPoly) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size

  if(arg.arity != scale.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorScaleInput(arg.arityOp(op), scale.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.scaleinput(arg.translate, scale)
}

case class AVectorScaleConstant(val arg: AVector, val scale: Double) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size

  def arityOp(op: ArityOp): AVector = AVectorScaleConstant(arg.arityOp(op), scale)

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.scaleconstant(arg.translate, scale)
}

case class AVectorCat(val arg1: AVector, val arg2: AVector) extends AVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size + arg2.size

  def arityOp(op: ArityOp): AVector = AVectorCat(arg1.arityOp(op), arg2.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.cat(arg1.translate, arg2.translate)
}

case class AVectorCatFor(val len: IRPoly, val arg: AVector) extends AVector {
  val arity: Int = len.arity
  val size: IRPoly = arg.size.sum(arity).substituteAt(arity, len)

  if(len.arity + 1 != arg.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorCatFor(len.arityOp(op), arg.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.catfor(len, arg.translate)
}

case class AVectorSlice(val arg: AVector, val at: IRPoly, val size: IRPoly) extends AVector {
  val arity: Int = size.arity

  if(arg.arity != arity) throw new IRValidationException()
  if(at.arity != arity) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorSlice(arg.arityOp(op), at.arityOp(op), size.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.slice(arg.translate, at, size)
}

case class AVectorAddFor(val len: IRPoly, val arg: AVector) extends AVector {
  val arity: Int = len.arity
  val size: IRPoly = arg.size.demote

  if(arg.arity != len.arity + 1) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorAddFor(len.arityOp(op), arg.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.addfor(len, arg.translate)
}
