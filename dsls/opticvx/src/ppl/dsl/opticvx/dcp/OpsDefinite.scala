package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._
import ppl.dsl.opticvx.solverir._

trait DCPOpsDefinite extends DCPOps {

/*  
  type ParamDesc = Int
  type InputDesc = InputDescDefinite
  type ExprRT = Seq[Double]

  case class InputDescDefinite(val size: IRPoly, val data: (Int)=>Double) extends HasSize

  implicit def double2inputdesc(x: Double) = InputDescDefinite(IRPoly.const(1, globalArity),
    (i: Int) => {
      if(i != 0) throw new IRValidationException()
      x
      })

  def vector_input(size: Int)(data: (Int)=>Double) = InputDescDefinite(IRPoly.const(size, globalArity), data)
  def vector_input(size: IRPoly)(data: (Int)=>Double) = InputDescDefinite(size, data)

  override def postsolve(problem: Problem, params: Seq[Int], inputs: Seq[InputDescDefinite], syms: Seq[Symbol[Expr, ExprRT]]) {
    val tt = PrimalDualSubgradient.Gen(problem).solver

    var vvinputs: Seq[Double] = Seq()
    for (i <- inputs) {
      vvinputs = vvinputs ++ ((0 until i.size.eval(params)(IntLikeInt)) map i.data)
    }

    val vv = tt.run(params, vvinputs)

    for(s <- syms) {
      val x = s.binding
      val scontext = SolverContext(tt.input, Seq(tt.variables(0)))
      val avlsv = AVectorLikeSVector(scontext)
      val sv = SVectorAdd(
        x.offset.translate(avlsv),
        x.almap.mmpy(SVectorRead(scontext, 0): SVector)(avlsv))
      s.rset(sv.eval(params, vvinputs, Seq(vv(0))))
    }
  }
*/
}
