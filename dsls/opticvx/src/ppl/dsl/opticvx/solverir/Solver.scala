package ppl.dsl.opticvx.solverir

import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

case class Solver(
  val input: InputDesc,
  val variables: Seq[IRPoly],
  val code: Seq[SolverInstr]) extends HasInput[Solver]
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
  def inputOp(op: InputOp) = Solver(op.input, variables, code map (c => c.inputOp(op)))

  def run(params: Seq[Int], inputs: Seq[Double]): Seq[Seq[Double]] = {
    //if (inputs.size != input.eval(params)(IntLikeInt)) throw new IRValidationException()
    var memory: Seq[Seq[Double]] = variables map (p => null)
    for (i <- code) {
      memory = i.run(params, inputs, memory)
    }
    memory
  }
}

case class SolverContext(val input: InputDesc, val variables: Seq[IRPoly]) extends HasInput[SolverContext] {
  val arity: Int = input.arity
  
  for(v <- variables) {
    if(v.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = SolverContext(input.arityOp(op), variables map (v => v.arityOp(op)))
  def inputOp(op: InputOp) = SolverContext(op.input, variables)
}

trait SolverInstr extends HasInput[SolverInstr] {
  val context: SolverContext

  def run(params: Seq[Int], inputs: Seq[Double], memory: Seq[Seq[Double]]): Seq[Seq[Double]]
}

case class SolverInstrWrite(
  val context: SolverContext,
  val dst: Int,
  val src: SVector) extends SolverInstr
{
  val arity: Int = context.arity
  val input: InputDesc = context.input

  if((dst<0)||(dst>=context.variables.size)) throw new IRValidationException()
  if(src.context != context) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverInstrWrite(context.arityOp(op), dst, src.arityOp(op))
  def inputOp(op: InputOp) = SolverInstrWrite(context.inputOp(op), dst, src.inputOp(op))

  def run(params: Seq[Int], inputs: Seq[Double], memory: Seq[Seq[Double]]): Seq[Seq[Double]] 
    = memory.updated(dst, src.eval(params, inputs, memory))
}

case class SolverInstrConverge(
  val context: SolverContext,
  val condition: SVector,
  val code: Seq[SolverInstr]) extends SolverInstr
{
  val arity: Int = context.arity
  val input: InputDesc = context.input

  if(condition.context != context) throw new IRValidationException()
  if(condition.size != IRPoly.const(1, arity)) throw new IRValidationException()

  def arityOp(op: ArityOp) = 
    SolverInstrConverge(context.arityOp(op), condition.arityOp(op), code map (c => c.arityOp(op)))
  def inputOp(op: InputOp) = 
    SolverInstrConverge(context.inputOp(op), condition.inputOp(op), code map (c => c.inputOp(op)))

  def run(params: Seq[Int], inputs: Seq[Double], memory: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    var mem = memory
    var counter: Int = 0
    // hardcoded tolerance for now
    while(condition.eval(params, inputs, mem)(0) > 1e-8) {
      for(c <- code) {
        mem = c.run(params, inputs, mem)
      }
      counter += 1
    }
    println("converged in " + counter.toString + " iterations")
    mem
  }
}

