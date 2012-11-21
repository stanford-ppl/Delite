package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq

trait SolverGen {
  def gen(p: Problem): Solver
}

class PrimalDualSubgradientSolverGen extends SolverGen {
  def gen(p: Problem): Solver = {
    
  }
}