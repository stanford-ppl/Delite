package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait AVectorLike[T <: HasArity[T]] extends HasArity[AVectorLike[T]] {
  val arity: Int
  def size(arg: T): IRPoly
  def zero(size: IRPoly): T
  def one: T
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

case class AVectorLikeScale[T <: HasArity[T]](val base: T, val e: AVectorLike[T]) extends AVectorLike[T] {
  val arity: Int = e.arity
  def size(arg: T): IRPoly = e.size(arg) / e.size(base)
  def zero(size: IRPoly): T = e.zero(size * e.size(base))
  def one: T = base
  def add(arg1: T, arg2: T): T = e.add(arg1, arg2)
  def addfor(len: IRPoly, arg:T): T = e.addfor(len, arg)
  def neg(arg: T): T = e.neg(arg)
  def scaleinput(arg: T, scale: IRPoly): T = e.scaleinput(arg, scale)
  def scaleconstant(arg: T, scale: Double): T = e.scaleconstant(arg, scale)
  def cat(arg1: T, arg2:T): T = e.cat(arg1, arg2)
  def catfor(len: IRPoly, arg: T): T = e.catfor(len, arg)
  def slice(arg: T, at: IRPoly, size: IRPoly): T = e.slice(arg, at, size)

  def arityOp(op: ArityOp): AVectorLike[T] = AVectorLikeScale(base.arityOp(op), e.arityOp(op))
}

case class AVectorLikeAVector(val arity: Int) extends AVectorLike[AVector] {
  def size(arg: AVector): IRPoly = arg.size
  def zero(size: IRPoly): AVector = AVectorZero(size)
  def one: AVector = AVectorOne(arity)
  def add(arg1: AVector, arg2: AVector): AVector = AVectorAdd(arg1, arg2)
  def addfor(len: IRPoly, arg: AVector): AVector = AVectorAddFor(len, arg)
  def neg(arg: AVector): AVector = AVectorNeg(arg)
  def scaleinput(arg: AVector, scale: IRPoly): AVector = AVectorScaleInput(arg, scale)
  def scaleconstant(arg: AVector, scale: Double): AVector = AVectorScaleConstant(arg, scale)
  def cat(arg1: AVector, arg2: AVector): AVector = AVectorCat(arg1, arg2)
  def catfor(len: IRPoly, arg: AVector): AVector = AVectorCatFor(len, arg)
  def slice(arg: AVector, at: IRPoly, size: IRPoly): AVector = AVectorSlice(arg, at, size)

  def arityOp(op: ArityOp): AVectorLike[AVector] = AVectorLikeAVector(IRPoly.const(0, arity).arityOp(op).arity)
}


object AVector {
  def input(at: IRPoly, len: IRPoly): AVector = {
    if(at.arity != len.arity) throw new IRValidationException()
    AVectorCatFor(len, AVectorScaleInput(AVectorOne(at.arity + 1), at.promote + at.next))
  }
  def const(c: Double, arity: Int): AVector = {
    AVectorScaleConstant(AVectorOne(arity), c)
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
  def is0: Boolean
  def isPure: Boolean
}

case class AVectorZero(val size: IRPoly) extends AVector {
  val arity: Int = size.arity  
  def arityOp(op: ArityOp): AVector = AVectorZero(size.arityOp(op))
  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.zero(size)
  def is0: Boolean = true
  def isPure: Boolean = true
}

case class AVectorOne(val arity: Int) extends AVector {
  val size: IRPoly = IRPoly.const(1, arity)
  def arityOp(op: ArityOp): AVector = AVectorOne(size.arityOp(op).arity)
  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = {
    if (e.arity != arity) throw new IRValidationException()
    e.one
  }
  def is0: Boolean = false
  def isPure: Boolean = true
}

case class AVectorAdd(val arg1: AVector, val arg2: AVector) extends AVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size

  if(arg1.size != arg2.size) {
    println(arg1.size)
    println(arg2.size)
    throw new IRValidationException()
  }

  def arityOp(op: ArityOp): AVector = AVectorAdd(arg1.arityOp(op), arg2.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.add(arg1.translate, arg2.translate)

  def is0: Boolean = arg1.is0 && arg2.is0
  def isPure: Boolean = arg1.isPure && arg2.isPure
}

case class AVectorNeg(val arg: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size

  def arityOp(op: ArityOp): AVector = AVectorNeg(arg.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.neg(arg.translate)

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure
}

case class AVectorScaleInput(val arg: AVector, val scale: IRPoly) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size

  if(arg.arity != scale.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorScaleInput(arg.arityOp(op), scale.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.scaleinput(arg.translate, scale)

  def is0: Boolean = arg.is0
  def isPure: Boolean = false
}

case class AVectorScaleConstant(val arg: AVector, val scale: Double) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size

  def arityOp(op: ArityOp): AVector = AVectorScaleConstant(arg.arityOp(op), scale)

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.scaleconstant(arg.translate, scale)

  def is0: Boolean = arg.is0 || (scale == 0.0)
  def isPure: Boolean = arg.isPure
}

case class AVectorCat(val arg1: AVector, val arg2: AVector) extends AVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size + arg2.size

  def arityOp(op: ArityOp): AVector = AVectorCat(arg1.arityOp(op), arg2.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.cat(arg1.translate, arg2.translate)

  def is0: Boolean = arg1.is0 && arg2.is0
  def isPure: Boolean = arg1.isPure && arg2.isPure
}

case class AVectorCatFor(val len: IRPoly, val arg: AVector) extends AVector {
  val arity: Int = len.arity
  val size: IRPoly = arg.size.sum(arity).substituteAt(arity, len)

  if(len.arity + 1 != arg.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorCatFor(len.arityOp(op), arg.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.catfor(len, arg.translate(e.promote))

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure
}

case class AVectorSlice(val arg: AVector, val at: IRPoly, val size: IRPoly) extends AVector {
  val arity: Int = size.arity

  if(arg.arity != arity) throw new IRValidationException()
  if(at.arity != arity) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorSlice(arg.arityOp(op), at.arityOp(op), size.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.slice(arg.translate, at, size)

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure
}

case class AVectorAddFor(val len: IRPoly, val arg: AVector) extends AVector {
  val arity: Int = len.arity
  val size: IRPoly = arg.size.demote

  if(arg.arity != len.arity + 1) throw new IRValidationException()

  def arityOp(op: ArityOp): AVector = AVectorAddFor(len.arityOp(op), arg.arityOp(op))

  def translate[V <: HasArity[V]](implicit e: AVectorLike[V]): V = e.addfor(len, arg.translate(e.promote))
  
  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure
}
