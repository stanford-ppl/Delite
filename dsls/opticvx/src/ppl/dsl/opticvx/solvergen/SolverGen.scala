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

  case class SolverGenVector(val at: IRPoly, val len: IRPoly) extends SolverGenExpr {
    def :=(x: SolverGenExpr) = scaleSet(x, SolverGenExprConstant(0))
    def +=(x: SolverGenExpr) = scaleSet(x, SolverGenExprConstant(1))
    def -=(x: SolverGenExpr) = scaleSet(x, SolverGenExprConstant(-1))
    def scaleSet(x: SolverGenExpr, dstscale: SolverGenExpr) {
      if (context == null) {
        scratchSize = IRPoly.pmax(scratchSize, x.scratch)
      }
      else {
        code = code ++ x.put(this, dstscale)
      }
    }
    def put(dst: SolverGenVector, dstscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      AlmapIdentity(inputSize, len).genmmpy(
        context, at, dst.at, memorySize, SolverExprConstant(context, 1), dstscale.get)
    }
    def get: SolverExpr = throw new IRValidationException()
    def scratch: IRPoly = IRPoly.const(0, arity)
  }

  trait SolverGenExpr {
    val len: IRPoly
    def put(dst: SolverGenVector, dstscale: SolverGenExpr): Seq[SolverInstr]
    def get: SolverExpr
    def scratch: IRPoly
  }

  case class SolverGenExprConstant(c: Double) extends SolverGenExpr {
    val len: IRPoly = IRPoly.const(1, arity)
    def put(dst: SolverGenVector, dstscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      Seq(SolverInstrWrite(context, dst.at, SolverExprBinaryOp(context, SolverBinaryOpMpy, get, dstscale.get)))
    }
    def get: SolverExpr = SolverExprConstant(context, c)
    def scratch: IRPoly = IRPoly.const(0, arity)
  }

  case class SolverGenExprMMpy(almap: Almap, v: SolverGenVector) extends SolverGenExpr {
    val len: IRPoly = almap.codomain
    def put(dst: SolverGenVector, dstscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      almap.genmmpy(
        context, v.at, dst.at, memorySize, SolverExprConstant(context, 1), dstscale.get)
    }
    def get: SolverExpr = throw new IRValidationException()
    def scratch: IRPoly = almap.scratch
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

}
