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

  trait SolverGenExpr {
    val len: IRPoly
    def put(dst: SolverGenVector, srcscale: SolverGenExpr, dstscale: SolverGenExpr): Seq[SolverInstr]
    def get: SolverExpr
    def scratch: IRPoly
  }

  case class SolverGenVector(val at: IRPoly, val len: IRPoly) extends SolverGenExpr {
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
    def :=(x: SolverGenExpr) = scaleSet(x, SolverGenExprConstant(0), SolverGenExprConstant(1))
    def +=(x: SolverGenExpr) = scaleSet(x, SolverGenExprConstant(1), SolverGenExprConstant(1))
    def -=(x: SolverGenExpr) = scaleSet(x, SolverGenExprConstant(-1), SolverGenExprConstant(1))
    def scaleSet(x: SolverGenExpr, dstscale: SolverGenExpr, srcscale: SolverGenExpr) {
      if (context == null) {
        scratchSize = IRPoly.pmax(scratchSize, x.scratch)
      }
      else {
        code = code ++ x.put(this, dstscale, srcscale)
      }
    }
    def put(dst: SolverGenVector, dstscale: SolverGenExpr, srcscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(srcscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      AlmapIdentity(inputSize, len).genmmpy(
        context, at, dst.at, memorySize, srcscale.get, dstscale.get)
    }
    def get: SolverExpr = {
      if (len == IRPoly.const(1, arity)) SolverExprRead(context, at)
      else throw new IRValidationException()
    }
    def scratch: IRPoly = IRPoly.const(0, arity)
  }

  case class SolverGenExprConstant(c: Double) extends SolverGenExpr {
    val len: IRPoly = IRPoly.const(1, arity)
    def put(dst: SolverGenVector, dstscale: SolverGenExpr, srcscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(srcscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      Seq(
        SolverInstrWrite(
          context, 
          dst.at, 
          SolverExprBinaryOp(
            context,
            SolverBinaryOpAdd,
            SolverExprBinaryOp(
              context, 
              SolverBinaryOpMpy, 
              srcscale.get, 
              get),
            SolverExprBinaryOp(
              context,
              SolverBinaryOpMpy,
              dstscale.get,
              SolverExprRead(
                context,
                dst.at)))))
    }
    def get: SolverExpr = SolverExprConstant(context, c)
    def scratch: IRPoly = IRPoly.const(0, arity)
  }

  case class SolverGenExprMMpy(almap: Almap, v: SolverGenVector) extends SolverGenExpr {
    val len: IRPoly = almap.codomain
    def put(dst: SolverGenVector, dstscale: SolverGenExpr, srcscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(srcscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      almap.genmmpy(
        context, v.at, dst.at, memorySize, srcscale.get, dstscale.get)
    }
    def get: SolverExpr = throw new IRValidationException()
    def scratch: IRPoly = almap.scratch
  }

  case class SolverGenExprAdd(x: SolverGenExpr, y: SolverGenExpr) extends SolverGenExpr {
    if(x.len != y.len) throw new IRValidationException()
    val len: IRPoly = x.len
    def scratch: IRPoly = IRPoly.pmax(x.scratch, y.scratch)
    def get: SolverExpr = throw new IRValidationException()
    def put(dst: SolverGenVector, dstscale: SolverGenExpr, srcscale: SolverGenExpr): Seq[SolverInstr] = {
      if(dst.len != len) throw new IRValidationException()
      if(srcscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      if(dstscale.len != IRPoly.const(1, arity)) throw new IRValidationException()
      x.put(dst, dstscale, srcscale) ++ y.put(dst, SolverGenExprConstant(1), srcscale)
    }
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
