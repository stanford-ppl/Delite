package ppl.dsl.opticvx.solverir

import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait Solver extends HasInput[Solver] {
  
}

/*
case class SolverContext(val input: InputDesc, val variables: Seq[IRPoly]) extends HasInput[SolverContext] {
  val arity: Int = input.arity
  
  for(v <- variables) {
    if(v.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp) = SolverContext(input.arityOp(op), variables map (v => v.arityOp(op)))
  def inputOp(op: InputOp) = SolverContext(op.input, variables)
}
*/

case class SolverWrite(val src: AVector, val iidx: Int, val sidx: Seq[IRPoly]) extends Solver {
  val arity: Int = src.arity
  val input: InputDesc = src.input

  if(input.memory(iidx).size.substituteSeq(sidx) != src.size) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverWrite(src.arityOp(op), iidx, sidx map (s => s.arityOp(op)))
  def inputOp(op: InputOp) = SolverWrite(src.inputOp(op), iidx, sidx)
}

case class SolverConverge(val condition: AVector, val body: Solver) extends Solver {
  val arity: Int = condition.arity
  val input: InputDesc = condition.input

  if(condition.size != IRPoly.const(1, arity)) throw new IRValidationException()
  if(body.input != input) throw new IRValidationException()

  def arityOp(op: ArityOp) = 
    SolverConverge(condition.arityOp(op), body.arityOp(op))
  def inputOp(op: InputOp) = 
    SolverConverge(condition.inputOp(op), body.inputOp(op))
}


