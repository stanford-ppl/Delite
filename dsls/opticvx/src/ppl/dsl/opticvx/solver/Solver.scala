package ppl.dsl.opticvx.solver

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

case class Solver(
  val input: IRPoly,
  val memory: IRPoly,
  val code: Seq[SolverInstr]) extends HasArity[Solver]
{
  val arity: Int = input.arity

  if(input.arity != arity) throw new IRValidationException()
  if(memory.arity != arity) throw new IRValidationException()
  for(c <- code) {
    if(c.context != SolverContext(input, memory, Seq())) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = Solver(input.arityOp(op), memory.arityOp(op), code map (c => c.arityOp(op)))
}

case class SolverContext(val input: IRPoly, val memory: IRPoly, val limits: Seq[IRPoly]) extends HasArity[SolverContext] {
  val arity: Int = input.arity
  val innerarity: Int = input.arity + limits.length
  
  if(input.arity != arity) throw new IRValidationException()
  if(memory.arity != arity) throw new IRValidationException()
  for(i <- 0 until limits.length) {
    if(limits(i).arity != arity + i) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = SolverContext(input.arityOp(op), memory.arityOp(op), limits map (l => l.arityOp(op)))

  def pushLimit(l: IRPoly) = SolverContext(input, memory, limits :+ l)
  def popLimit = SolverContext(input, memory, limits.init)

  def memoryInBounds(at: IRPoly): Boolean = isNonNegative(at) && isNonNegative(memory.promoteBy(limits.length) - at - IRPoly.const(1, innerarity))
  def inputInBounds(at: IRPoly): Boolean = isNonNegative(at) && isNonNegative(input.promoteBy(limits.length) - at - IRPoly.const(1, innerarity))

  // is the given irpoly always nonnegative over any positive input params, with bound params
  // that satisfy the given limits?  note that this function may have false negatives
  def isNonNegative(irpoly: IRPoly): Boolean = {
    if(irpoly.arity != innerarity) throw new IRValidationException()
    val dp = irpoly.diff(innerarity - 1)
    if(dp.isNonNegative) {
      // this polynomial is nondecreasing in Z
      // therefore, it will be positive over the region iff it is positive at Z = 0
      popLimit.isNonNegative(irpoly.substituteAt(innerarity - 1, IRPoly.const(0, innerarity - 1)))
    }
    else if((-dp).isNonNegative) {
      // this polynomial is nonincreasing in Z
      // therefore, it will be positive over the region iff it is positive at Z = L - 1
      popLimit.isNonNegative(irpoly.substituteAt(innerarity - 1, limits.last - IRPoly.const(1, innerarity - 1)))
    }
    else {
      // this is neither nonincreasing or nondecreasing, so assume that it could be negative
      false
    }
  }
}

trait SolverInstr extends HasArity[SolverInstr] {
  val context: SolverContext
}

trait SolverExpr extends HasArity[SolverExpr] {
  val context: SolverContext
}

case class SolverInstrWrite(
  val context: SolverContext,
  val at: IRPoly,
  val expr: SolverExpr
  ) extends SolverInstr
{
  val arity: Int = context.arity

  if(at.arity != context.innerarity) throw new IRValidationException()
  if(!(context.memoryInBounds(at))) throw new IRValidationException()
  if(expr.context != context) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverInstrWrite(context.arityOp(op), at.arityOp(op), expr.arityOp(op))
}

case class SolverInstrIterate(
  val context: SolverContext,
  val cond: SolverExpr, //condition must be positive for looping to continue
  val iterlimit: IRPoly,
  val code: Seq[SolverInstr]
  ) extends SolverInstr
{
  val arity: Int = context.arity

  if (cond.context != context) throw new IRValidationException()
  if (iterlimit.arity != context.innerarity) throw new IRValidationException()
  for (c <- code) {
    if (c.context != context) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = SolverInstrIterate(context.arityOp(op), cond.arityOp(op), iterlimit.arityOp(op), code map (c => c.arityOp(op)))
}

case class SolverInstrParallel(
  val context: SolverContext,
  val body: Seq[Seq[SolverInstr]]
  ) extends SolverInstr
{
  val arity: Int = context.arity

  for(b <- body) {
    for (c <- b) {
      if (c.context != context) throw new IRValidationException()
    }
  }

  def arityOp(op: ArityOp) = SolverInstrParallel(context.arityOp(op), body map (b => b map (c => c.arityOp(op))))
}

case class SolverInstrParFor(
  val context: SolverContext,
  val len: IRPoly,
  val code: Seq[SolverInstr]
  ) extends SolverInstr
{
  val arity: Int = context.arity

  if(len.arity != context.innerarity) throw new IRValidationException()
  for (c <- code) {
    if (c.context != context.pushLimit(len)) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = SolverInstrParFor(context.arityOp(op), len.arityOp(op), code map (c => c.arityOp(op)))
}


case class SolverExprRead(
  val context: SolverContext,
  val at: IRPoly
  ) extends SolverExpr
{
  val arity: Int = context.arity

  if(at.arity != context.innerarity) throw new IRValidationException()
  if(!(context.memoryInBounds(at))) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverExprRead(context.arityOp(op), at.arityOp(op))
}

case class SolverExprInput(
  val context: SolverContext,
  val at: IRPoly
  ) extends SolverExpr
{
  val arity: Int = context.arity

  if(at.arity != context.innerarity) throw new IRValidationException()
  if(!(context.inputInBounds(at))) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverExprRead(context.arityOp(op), at.arityOp(op))
}

case class SolverExprConstant(
  val context: SolverContext,
  val const: Double
  ) extends SolverExpr
{
  val arity: Int = context.arity

  def arityOp(op: ArityOp) = SolverExprConstant(context.arityOp(op), const)
}

case class SolverExprParReduce(
  val context: SolverContext,
  val len: IRPoly,
  val body: SolverExpr
  ) extends SolverExpr
{
  val arity: Int = context.arity

  if(len.arity != context.innerarity) throw new IRValidationException()
  if(body.context != context.pushLimit(len)) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverExprParReduce(context.arityOp(op), len.arityOp(op), body.arityOp(op))
}

case class SolverExprUnaryOp(
  val context: SolverContext,
  val op: SolverUnaryOp,
  val arg: SolverExpr
  ) extends SolverExpr
{
  val arity: Int = context.arity

  if(arg.context != context) throw new IRValidationException()

  def arityOp(aop: ArityOp) = SolverExprUnaryOp(context.arityOp(aop), op, arg.arityOp(aop))
}

case class SolverExprBinaryOp(
  val context: SolverContext,
  val op: SolverBinaryOp,
  val arg1: SolverExpr,
  val arg2: SolverExpr
  ) extends SolverExpr
{
  val arity: Int = context.arity

  if(arg1.context != context) throw new IRValidationException()
  if(arg2.context != context) throw new IRValidationException()

  def arityOp(aop: ArityOp) = SolverExprBinaryOp(context.arityOp(aop), op, arg1.arityOp(aop), arg2.arityOp(aop))
}

trait SolverUnaryOp
trait SolverBinaryOp

object SolverUnaryOpNeg extends SolverUnaryOp
object SolverUnaryOpSqrt extends SolverUnaryOp
object SolverUnaryOpExp extends SolverUnaryOp
object SolverUnaryOpLog extends SolverUnaryOp

object SolverBinaryOpAdd extends SolverBinaryOp
object SolverBinaryOpMpy extends SolverBinaryOp
object SolverBinaryOpDiv extends SolverBinaryOp
object SolverBinaryOpMax extends SolverBinaryOp
object SolverBinaryOpMin extends SolverBinaryOp

