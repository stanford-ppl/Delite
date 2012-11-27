package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


case class PrimalDualSubgradientSolverGen(val problem: Problem) extends SolverGen {
  val x = vector(varSize)
  val v = vector(affineCstrtSize)
  val y = vector(coneSize)
  val Axb = vector(affineCstrtSize)
  val Fxg = vector(coneSize)
  val theta = scalar

  def gen {
    x := 0
    v := 0
    y := 0
    converge(x) {
      Axb := A*x + b
      Fxg := F*x + g
    }
  }
}