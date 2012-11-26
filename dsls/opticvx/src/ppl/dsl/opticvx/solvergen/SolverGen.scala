package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq

trait SolverGen {
  val problem: Problem

  val A: Almap = problem.affineAlmap
  val b: Almap = problem.affineOffset
  val F: Almap = problem.conicAlmap
  val g: Almap = problem.conicOffset
  val c: Almap = problem.objective
  val cone: Cone = problem.conicCone

  val arity = problem.arity
  val varSize = problem.varSize
  val inputSize = problem.inputSize
  val affineCstrtSize = problem.affineCstrtSize
  val coneSize = problem.coneSize

  private var memorySize: IRPoly = IRPoly.const(0, arity)

  def scalar: SolverGenVector = vector(IRPoly.const(1, arity))
  def vector(len: IRPoly): SolverGenVector = {
    val rv = SolverGenVector(memorySize, len)
    memorySize += len
    rv
  }

  private var code: Seq[SolverInstr] = null
  private var context: SolverContext = null
  private var scratchSize: IRPoly = null

  case class SolverGenVector(val at: IRPoly, val len: IRPoly) {
    def :=(zero: Int) {
      if(zero != 0) throw new IRValidationException()
      if (context != null) {
        code = code ++ Seq(
          SolverInstrParFor(
            context.pushLimit(len), 
            len, 
            Seq(
              SolverInstrWrite(
                context.pushLimit(len),
                IRPoly.param(arity, arity+1),
                SolverExprConstant(
                  context.pushLimit(len),
                  0)))))
      }
    }    
    def :=(almap: Almap) {

    }
    def +=(almap: Almap) {
      this := (this + almap)
    }
    def -=(almap: Almap) {
      this := (this - almap)
    }
  }

  implicit def vector2almap(v: SolverGenVector): Almap = {
    AlmapVCatFor(v.len, AlmapScaleMemory(AlmapIdentity(IRPoly.const(1, v.len.arity)), v.at))
  }


  def gen(): Unit

  def solver(): Solver = {
    scratchSize = IRPoly.const(0, arity)
    gen()
    code = Seq()
    context = SolverContext(inputSize, memorySize + scratchSize, Seq())
    gen()
    Solver(inputSize, memorySize + scratchSize, code)
  }

  def converge(body: =>Unit) {

  }
}
