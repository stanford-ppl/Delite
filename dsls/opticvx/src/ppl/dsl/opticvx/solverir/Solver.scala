package ppl.dsl.opticvx.solverir

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

case class Solver(
  val input: IRPoly,
  val variables: Seq[IRPoly],
  val code: Seq[SolverInstr]) extends HasArity[Solver]
{
  val arity: Int = input.arity

  if(input.arity != arity) throw new IRValidationException()
  for(v <- variables) {
    if(v.arity != arity) throw new IRValidationException()
  }
  for(c <- code) {
    if(c.context != SolverContext(input, variables)) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = Solver(input.arityOp(op), variables map (v => v.arityOp(op)), code map (c => c.arityOp(op)))

  def run(params: Seq[Int], inputs: Seq[Double]): Seq[Seq[Double]] = {
    if (inputs.size != input.eval(params)(IntLikeInt)) throw new IRValidationException()
    val memory: Seq[Array[Double]] = variables map (p => new Array[Double](p.eval(params)(IntLikeInt)))
    for (i <- code) {
      i.run(params, inputs, memory)
    }
    memory map (a => Seq(a:_*))
  }
}

case class SolverContext(val input: IRPoly, val variables: Seq[IRPoly]) extends HasArity[SolverContext] {
  val arity: Int = input.arity
  
  if(input.arity != arity) throw new IRValidationException()
  for(v <- variables) {
    if(v.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = SolverContext(input.arityOp(op), variables map (v => v.arityOp(op)))
}

trait SolverInstr extends HasArity[SolverInstr] {
  val context: SolverContext
}

case class SolverInstrWrite(
  val context: SolverContext,
  val dst: Int,
  val src: SVector) extends SolverInstr
{
  val arity: Int = context.arity

  if((dst<0)||(dst>=context.variables.size)) throw new IRValidationException()
  if(src.context != context) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverInstrWrite(context.arityOp(op), dst, src.arityOp(op))
}

case class SolverInstrConverge(
  val context: SolverContext,
  val condition: SVector,
  val code: Seq[SolverInstr]) extends SolverInstr
{
  val arity: Int = context.arity

  if(condition.context != context) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverInstrConverge(context.arityOp(op), condition.arityOp(op), code map (c => c.arityOp(op)))
}

