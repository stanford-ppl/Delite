package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
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

  def gen(): Unit

  def solver(): Solver = {
    code = Seq()
    context = SolverContext(inputSize, variables)
    gen()
    Solver(inputSize, variables, code)
  }

  def converge(body: =>Unit) {

  }
}
