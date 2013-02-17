package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait SolverRuntime[I, M, N, V, W] extends IntLike[I] {
  //VECTOR OPERATIONS
  //base objects
  def size(arg: V): I
  def zero(size: I): V
  def one: V
  //linear operators
  def sum(arg1: V, arg2: V): V
  def sumfor(len: I, arg: (I => V)): V
  def neg(arg: V): V
  def scaleconstant(arg: V, scale: Double): V
  def cat(arg1: V, arg2: V): V
  def catfor(len: I, arg: (I => V)): V
  def slice(arg: V, at: I, size: I): V
  //nonlinear operators
  def dot(arg1: V, arg2: V): V
  def mpy(arg: V, scale: V): V
  def div(arg: V, scale: V): V
  def norm2(arg: V): V
  def sqrt(arg: V): V
  def max(arg1: V, arg2: V): V
  def min(arg1: V, arg2: V): V


  def matrixmpy(m: M, x: V): V
  def matrixmpytranspose(m: M, x: V): V

  def matrixget(mats: N, at: Seq[I]): M

  def vectorget(vecs: W, at: Seq[I]): V
  def vectorset(src: V, vecs: W, at: Seq[I]): W
}


trait Solver extends HasInput[Solver] {
  def run[I, M, N, V, W](runtime: SolverRuntime[I, M, N, V, W], params: Seq[I], inputs: Seq[N], memory: Seq[W]): Seq[W]
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

case class SolverNull(val input: InputDesc) extends Solver {
  val arity: Int = input.arity

  def arityOp(op: ArityOp) = SolverNull(input.arityOp(op))
  def inputOp(op: InputOp) = SolverNull(op.input)

  def run[I, M, N, V, W](runtime: SolverRuntime[I, M, N, V, W], params: Seq[I], inputs: Seq[N], memory: Seq[W]): Seq[W] =
  {
    memory
  }
}

case class SolverWrite(val src: AVector, val iidx: Int, sidx: Seq[IRPoly]) extends Solver {
  val arity: Int = src.arity
  val input: InputDesc = src.input

  def arityOp(op: ArityOp) = SolverWrite(src.arityOp(op), iidx, sidx map (s => s.arityOp(op)))
  def inputOp(op: InputOp) = SolverWrite(src.inputOp(op), iidx, sidx)

  def run[I, M, N, V, W](runtime: SolverRuntime[I, M, N, V, W], params: Seq[I], inputs: Seq[N], memory: Seq[W]): Seq[W] =
  {
    val eval_src = src.eval(runtime, params, inputs, memory)
    val set_res = runtime.vectorset(eval_src, memory(iidx), sidx map (s => s.eval(params)(runtime)))
    for(i <- 0 until memory.length) yield if (i == iidx) set_res else memory(i)
  }
}

case class SolverConverge(val condition: AVector, val body: Solver) extends Solver {
  val arity: Int = condition.arity
  val input: InputDesc = condition.input

  if(condition.size != IRPoly.const(1, arity)) throw new IRValidationException()
  if(body.input != input) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverConverge(condition.arityOp(op), body.arityOp(op))
  def inputOp(op: InputOp) = SolverConverge(condition.inputOp(op), body.inputOp(op))
}

case class SolverSeq(val first: Solver, val second: Solver) extends Solver {
  val arity: Int = first.arity
  val input: InputDesc = first.input

  if(first.input != second.input) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverSeq(first.arityOp(op), second.arityOp(op))
  def inputOp(op: InputOp) = SolverSeq(first.inputOp(op), second.inputOp(op))

  def run[I, M, N, V, W](runtime: SolverRuntime[I, M, N, V, W], params: Seq[I], inputs: Seq[N], memory: Seq[W]): Seq[W] =
  {
    second.run(runtime, params, inputs
      first.run(runtime, params, inputs, memory))
  }
}

// Sequential FOR
case class SolverSeqFor(val len: IRPoly, val body: Solver) extends Solver {
  val arity: Int = len.arity
  val input: InputDesc = body.input.demote

  if(len.arity + 1 != body.arity) throw new IRValidationException()

  def arityOp(op: ArityOp) = SolverSeqFor(len.arityOp(op), body.arityOp(op.promote))
  def inputOp(op: InputOp) = SolverSeqFor(len, body.inputOp(op.promote))
}
