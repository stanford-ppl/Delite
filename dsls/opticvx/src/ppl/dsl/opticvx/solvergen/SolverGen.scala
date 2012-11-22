package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq

trait SolverGen {
  val problem: Problem

  val arity = problem.arity
  val varSize = problem.varSize
  val inputSize = problem.inputSize
  val affineCstrtSize = problem.affineCstrtSize
  val coneSize = problem.coneSize

  def scalar: SolverGenVector = vector(IRPoly.const(1, arity))
  def vector(len: IRPoly): SolverGenVector

  case class SolverGenVector(val at: IRPoly, val len: IRPoly) {
    def :=(x: SolverGenExpr)
    def +=(x: SolverGenExpr)
    def -=(x: SolverGenExpr)
  }

  trait SolverGenExpr {
    val len: IRPoly
    def put(dst: SolverGenVector, dstscale: SolverGenExpr)
  }

  case class SolverGenExprMMpy(val almap: Almap, val x: SolverGenVector) {
    if(almap.domain != x.len) throw new IRValidationException()
    def put(dst: SolverGenVector)
  }

  def gen(p: Problem): Solver
}
