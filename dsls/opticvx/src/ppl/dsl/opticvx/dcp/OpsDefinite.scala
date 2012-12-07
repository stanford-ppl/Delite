package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._


trait DCPOpsDefinite extends DCPOps {
  
  type ParamDesc = Int
  type InputDesc = InputDescDefinite

  case class InputDescDefinite(val size: IRPoly, val data: (Int)=>Double) extends HasSize

  implicit def double2inputdesc(x: Double) = InputDescDefinite(IRPoly.const(1, globalArity),
    (i: Int) => {
      if(i != 0) throw new IRValidationException()
      x
      })

  override def postsolve(problem: Problem, params: Seq[Int], inputs: Seq[InputDescDefinite]) {
    val tt = PrimalDualSubgradient.Gen(problem).solver

    var vvinputs: Seq[Double] = Seq()
    for (i <- inputs) {
      vvinputs = vvinputs ++ ((0 until i.size.eval(params)(IntLikeInt)) map i.data)
    }

    val vv = tt.run(params, vvinputs)
  }
}