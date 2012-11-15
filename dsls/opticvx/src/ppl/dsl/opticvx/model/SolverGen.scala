package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq

trait SolverGen {
  def gen(p: Problem): Solver
}

class ADMMSolverGen extends SolverGen {
  def gen(p: Problem): Solver = {
    val scratchSize: IRPoly = IRPoly.pmax(IRPoly.pmax(IRPoly.pmax(IRPoly.pmax(p.affineAlmap.scratch, p.affineOffset.scratch), p.conicAlmap.scratch), p.conicOffset.scratch), p.objective.scratch)
    
  }
}