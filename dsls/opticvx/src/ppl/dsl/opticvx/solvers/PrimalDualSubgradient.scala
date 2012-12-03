package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualSubgradient extends SolverGenBase {
  trait Variables extends SGVariables {

    println("Variables constructor.")

    val x = vector(varSize)
    val v = vector(affineCstrtSize)
    val y = vector(coneSize)
    val Axb = vector(affineCstrtSize)
    val Fxg = vector(coneSize)
    val theta = scalar
  }
  trait Code extends SGCode {
    self: Variables =>

    println("Code constructor.")

    x := 0
    v := 0
    y := 0
    converge(theta) {
      Axb := A*x + b
      Fxg := F*x + g
    }
  }
  case class Gen(val problem: Problem) extends SGVariables with Variables with SGCode with Code with SGGen {
    println("Gen constructor.")
  }
}