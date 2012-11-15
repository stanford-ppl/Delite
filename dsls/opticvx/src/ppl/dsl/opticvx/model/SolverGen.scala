package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq

trait SolverGen {
  def gen(p: Problem): Solver
}

class ADMMSolverGen extends SolverGen {
  def gen(p: Problem): Solver = {
    // combine the matrices into a single matrix for ADMM
    val c: Almap = AlmapVCat(p.objective.T, AlmapZero(p.inputSize, IRPoly.const(1, p.arity), p.coneSize))
    val b: Almap = AlmapVCat(p.affineOffset, p.conicOffset)
    val A: Almap = AlmapHCat(AlmapVCat(p.affineAlmap, p.conicAlmap), 
        AlmapVCat(AlmapZero(p.inputSize, p.coneSize, p.affineAlmap.codomain),
            AlmapIdentity(p.inputSize, p.coneSize)))
    // determine the scratch size
    val scratchSize: IRPoly = IRPoly.pmax(IRPoly.pmax(A.scratch, b.scratch), c.scratch)
    // determine the required memory space
    throw new IRValidationException()
  }
}