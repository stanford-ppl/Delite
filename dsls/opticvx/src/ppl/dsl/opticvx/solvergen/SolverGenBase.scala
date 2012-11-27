package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import scala.collection.immutable.Seq

trait SolverGen {  
  val problem: Problem

  val A: Almap = problem.affineAlmap
  val b: SVector = problem.affineOffset.translate(AVectorLikeSVectorLocal)
  val F: Almap = problem.conicAlmap
  val g: SVector = problem.conicOffset.translate(AVectorLikeSVectorLocal)
  val c: SVector = problem.objective.translate(AVectorLikeSVectorLocal)
  val cone: Cone = problem.conicCone

  val arity = problem.arity
  val varSize = problem.varSize
  val inputSize = problem.inputSize
  val affineCstrtSize = problem.affineCstrtSize
  val coneSize = problem.coneSize

  private var variables: Seq[IRPoly] = Seq()

  def scalar: SVariable = vector(IRPoly.const(1, arity))
  def vector(len: IRPoly): SVariable = {
    variables = variables ++ Seq(len)
    SVariable(variables.size - 1)
  }

  private var code: Seq[SolverInstr] = null
  private var context: SolverContext = null  

  implicit object AVectorLikeSVectorLocal extends AVectorLike[SVector] {
    def size(arg: SVector): IRPoly = arg.size
    def zero(size: IRPoly): SVector = SVectorZero(context, size)
    def one(size: IRPoly): SVector = SVectorOne(context, size)
    def add(arg1: SVector, arg2: SVector): SVector = SVectorAdd(arg1, arg2)
    def addfor(len: IRPoly, arg: SVector): SVector = SVectorAddFor(len, arg)
    def neg(arg: SVector): SVector = SVectorNeg(arg)
    def scaleinput(arg: SVector, scale: IRPoly): SVector = SVectorScaleInput(arg, scale)
    def scaleconstant(arg: SVector, scale: Double): SVector = SVectorScaleConstant(arg, scale)
    def cat(arg1: SVector, arg2: SVector): SVector = SVectorCat(arg1, arg2)
    def catfor(len: IRPoly, arg: SVector): SVector = SVectorCatFor(len, arg)
    def slice(arg: SVector, at: IRPoly, size: IRPoly): SVector = SVectorSlice(arg, at, size)
  }
  import AVectorLikeSVectorLocal._

  case class SVariable(val idx: Int) {
    def :=(i: Int) {
      if(i == 0) {
        this := zero(variables(idx))
      }
      else if(i == 1) {
        this := one(variables(idx))
      }
      else {
        throw new IRValidationException()
      }
    }
    def :=(v: SVector) {
      if(context == null) throw new IRValidationException()
      code = code ++ Seq(SolverInstrWrite(context, idx, v))
    }
    def +=(v: SVector) {
      this := (variable2vector(this) + v)
    }
    def -=(v: SVector) {
      this := (variable2vector(this) - v)
    }
  }

  implicit def variable2vector(v: SVariable): SVector = {
    if(context == null) throw new IRValidationException()
    SVectorRead(context, v.idx)
  }

  // A hack to get around the double implicit in the almap-vector multiply
  class AlmapHackImpl(val almap: Almap) {
    def *(v: SVariable) = almap*variable2vector(v)
  }
  implicit def almap2almaphackimpl(almap: Almap) = new AlmapHackImpl(almap)
  
  // Import the arithmetic ops from the AVectorLikeSVectorLocal object
  implicit def svector2svectorhackimpl(s: SVector) = AVectorLikeSVectorLocal.t2thackimpl(s)

  def gen(): Unit

  def solver(): Solver = {
    code = Seq()
    context = SolverContext(inputSize, variables)
    gen()
    Solver(inputSize, variables, code)
  }

  def converge(condition: SVector)(body: =>Unit) {
    if(condition.size != IRPoly.const(1, arity)) throw new IRValidationException()
    val curcode: Seq[SolverInstr] = code
    code = Seq()
    body
    code = curcode ++ Seq(SolverInstrConverge(context, condition, code))
  }
}
