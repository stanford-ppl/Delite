package ppl.dsl.opticvx.solverir

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq


trait SVector extends HasArity[SVector] {
  val size: IRPoly
  val context: SolverContext
}

case class AVectorLikeSVector(val context: SolverContext) extends AVectorLike[SVector] {
  val arity: Int = context.arity
  def size(arg: SVector): IRPoly = arg.size
  def zero(size: IRPoly): SVector = SVectorZero(context, size)
  def one: SVector = SVectorOne(context)
  def add(arg1: SVector, arg2: SVector): SVector = SVectorAdd(arg1, arg2)
  def addfor(len: IRPoly, arg: SVector): SVector = SVectorAddFor(len, arg)
  def neg(arg: SVector): SVector = SVectorNeg(arg)
  def scaleinput(arg: SVector, scale: IRPoly): SVector = SVectorScaleInput(arg, scale)
  def scaleconstant(arg: SVector, scale: Double): SVector = SVectorScaleConstant(arg, scale)
  def cat(arg1: SVector, arg2: SVector): SVector = SVectorCat(arg1, arg2)
  def catfor(len: IRPoly, arg: SVector): SVector = SVectorCatFor(len, arg)
  def slice(arg: SVector, at: IRPoly, size: IRPoly): SVector = SVectorSlice(arg, at, size)

  def arityOp(op: ArityOp): AVectorLike[SVector] = AVectorLikeSVector(context.arityOp(op))
}

case class SVectorRead(val context: SolverContext, val idx: Int) extends SVector {
  val arity: Int = context.arity
  val size: IRPoly = context.variables(idx)

  if((idx<0)||(idx>=context.variables.size)) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorRead(context.arityOp(op), idx)
}

case class SVectorZero(val context: SolverContext, val size: IRPoly) extends SVector {
  val arity: Int = size.arity  
  def arityOp(op: ArityOp): SVector = SVectorZero(context.arityOp(op), size.arityOp(op))
}

case class SVectorOne(val context: SolverContext) extends SVector {
  val arity: Int = context.arity
  val size: IRPoly = IRPoly.const(1, arity)
  def arityOp(op: ArityOp): SVector = SVectorOne(context.arityOp(op))
}

case class SVectorAdd(val arg1: SVector, val arg2: SVector) extends SVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size
  val context: SolverContext = arg1.context

  if(arg1.size != arg2.size) throw new IRValidationException()
  if(arg1.context != arg2.context) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorAdd(arg1.arityOp(op), arg2.arityOp(op))
}

case class SVectorNeg(val arg: SVector) extends SVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size
  val context: SolverContext = arg.context

  def arityOp(op: ArityOp): SVector = SVectorNeg(arg.arityOp(op))
}

case class SVectorScaleInput(val arg: SVector, val scale: IRPoly) extends SVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size
  val context: SolverContext = arg.context

  if(arg.arity != scale.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorScaleInput(arg.arityOp(op), scale.arityOp(op))
}

case class SVectorScaleConstant(val arg: SVector, val scale: Double) extends SVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size
  val context: SolverContext = arg.context

  def arityOp(op: ArityOp): SVector = SVectorScaleConstant(arg.arityOp(op), scale)
}

case class SVectorCat(val arg1: SVector, val arg2: SVector) extends SVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size + arg2.size
  val context: SolverContext = arg1.context
  
  if(arg1.context != arg2.context) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorCat(arg1.arityOp(op), arg2.arityOp(op))
}

case class SVectorCatFor(val len: IRPoly, val arg: SVector) extends SVector {
  val arity: Int = len.arity
  val size: IRPoly = arg.size.sum(arity).substituteAt(arity, len)
  val context: SolverContext = arg.context.demote

  if(len.arity + 1 != arg.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorCatFor(len.arityOp(op), arg.arityOp(op))
}

case class SVectorSlice(val arg: SVector, val at: IRPoly, val size: IRPoly) extends SVector {
  val arity: Int = size.arity
  val context: SolverContext = arg.context

  if(arg.arity != arity) throw new IRValidationException()
  if(at.arity != arity) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorSlice(arg.arityOp(op), at.arityOp(op), size.arityOp(op))
}

case class SVectorAddFor(val len: IRPoly, val arg: SVector) extends SVector {
  val arity: Int = len.arity
  val size: IRPoly = arg.size.demote
  val context: SolverContext = arg.context.demote

  if(arg.arity != len.arity + 1) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorAddFor(len.arityOp(op), arg.arityOp(op))
}