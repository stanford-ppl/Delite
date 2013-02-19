package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import scala.collection.immutable.Seq

trait SolverGen {

  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone): Unit

  private var input: InputDesc = null
  private var variables: Seq[MemoryArgDesc] = null
  private var vidx: Int = 0
  private var prephase: Boolean = true
  private var solveracc: Solver = null

  case class SVariable(iidx: Int) {
    def apply(sar: IRPoly*): SVariableEntry = SVariableEntry(iidx, Seq(sar:_*))
  }

  case class SVariableEntry(iidx: Int, sidx: Seq[IRPoly]) {
    if(variables(iidx).dims.length != sidx.length) throw new IRValidationException()

    def :=(src: AVector) {
      if(!prephase) {
        solveracc = SolverSeq(solveracc, SolverWrite(src, iidx, sidx))
      }
    }
    def +=(src: AVector) {
      this := svariableentry2vectorimpl(this) + src
    }
    def -=(src: AVector) {
      this := svariableentry2vectorimpl(this) - src
    }
  }

  implicit def svariable2svariableentryimpl(s: SVariable): SVariableEntry = {
    if(variables(s.iidx).dims.length != 0) throw new IRValidationException()
    SVariableEntry(s.iidx, Seq())
  }

  implicit def svariable2vectorimpl(s: SVariable): AVector = svariableentry2vectorimpl(s())

  implicit def svariableentry2vectorimpl(s: SVariableEntry): AVector = {
    if(prephase) {
      // in prephase, all reads result in zero because the memory isn't defined
      AVectorCatFor(variables(s.iidx).size.substituteSeq(s.sidx), AVectorOne(input.promote))
    }
    else {
      AVectorRead(input, s.iidx, s.sidx)
    }
  }

  def v2m(v: AVector): Almap = AlmapVector(v)

  def zeros(d: IRPoly, c: IRPoly): Almap = AlmapZero(input, d, c)
  def eye(d: IRPoly): Almap = AlmapIdentity(input, d)
  def zeros(l: IRPoly): AVector = AVectorZero(input, l)
  def ones(l: IRPoly): AVector = AVectorCatFor(l, AVectorOne(input.promote))

  def norm2(arg: AVector) = AVectorNorm2(arg)
  def sqrt(arg: AVector) = AVectorSqrt(arg)
  def dot(arg1: AVector, arg2: AVector) = AVectorDot(arg1, arg2)

  implicit def double2vector(x: Double): AVector = 
    AVectorScaleConstant(AVectorOne(input), x)

  def scalar: SVariable = vector(IRPoly.const(1, input.arity))
  def vector(len: IRPoly): SVariable = {
    if(prephase) {
      variables :+= MemoryArgDesc(Seq(), len)
      SVariable(variables.length - 1)
    }
    else {
      if(variables(vidx) != MemoryArgDesc(Seq(), len)) throw new IRValidationException()
      vidx += 1
      SVariable(vidx - 1)
    }
  }

  def converge(condition: AVector)(body: =>Unit) {
    if(condition.size != IRPoly.const(1, input.arity)) throw new IRValidationException()
    if(prephase) {
      body
    }
    else {
      val cursolver: Solver = solveracc
      solveracc = SolverNull(input)
      body
      solveracc = SolverSeq(cursolver, SolverConverge(condition, solveracc))
    }
  }

  def gen(problem: Problem): Solver = {
    if(!problem.isMemoryless) throw new IRValidationException()

    val A: Almap = problem.affineAlmap
    val b: AVector = problem.affineOffset
    val F: Almap = problem.conicAlmap
    val g: AVector = problem.conicOffset
    val c: AVector = problem.objective
    val cone: Cone = problem.conicCone

    input = problem.input
    variables = Seq()
    prephase = true
    solveracc = null

    code(A, b, F, g, c, cone)

    input = InputDesc(problem.arity, problem.input.args, variables)
    prephase = false
    vidx = 0
    solveracc = SolverNull(input)

    code(A.addMemory(variables), b.addMemory(variables), F.addMemory(variables), g.addMemory(variables), c.addMemory(variables), cone)

    if(vidx != variables.length) throw new IRValidationException()

    val rv = solveracc

    input = null
    variables = null
    prephase = true
    solveracc = null
    
    rv
  }
}

/*
trait SolverGenBase {
  type Variables <: SGVariables
  type Code <: SGCode
  type Gen <: SGGen

  trait SGVariables {
    val problem: Problem

    type VariableType

    val arity = problem.arity
    val varSize = problem.varSize
    val inputSize = problem.input
    val affineCstrtSize = problem.affineCstrtSize
    val coneSize = problem.coneSize

    protected[SolverGenBase] var variables: Seq[IRPoly] = Seq()

    def scalar: VariableType = vector(IRPoly.const(1, arity))
    def vector(len: IRPoly): VariableType = {
      variables = variables ++ Seq(len)
      make_variable(variables.size - 1)
    }

    def make_variable(index: Int): VariableType

    implicit def int2irpolyimpl(i: Int): IRPoly = IRPoly.const(i, arity)
  }

  trait SGCode {
    self: Variables =>

    type VariableType = SVariable
    def make_variable(index: Int): VariableType = SVariable(index)

    protected[SolverGenBase] val context: SolverContext = SolverContext(inputSize, variables)
    protected[SolverGenBase] var code: Seq[SolverInstr] = Seq()

    implicit val avlsvl: AVectorLike[SVector] = AVectorLikeSVector(context)

    val A: Almap = problem.affineAlmap
    val b: SVector = problem.affineOffset.translate
    val bm: Almap = AlmapVector(problem.affineOffset)
    val F: Almap = problem.conicAlmap
    val g: SVector = problem.conicOffset.translate
    val gm: Almap = AlmapVector(problem.conicOffset)
    val c: SVector = problem.objective.translate
    val cm: Almap = AlmapVector(problem.objective)
    val cone: Cone = problem.conicCone

    case class SVariable(val idx: Int) {
      def :=(i: Int) {
        if(i == 0) {
          this := avlsvl.zero(variables(idx))
        }
        else if(i == 1) {
          this := avlsvl.one
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
        this := avlsvl.sum(variable2vector(this), v)
      }
      def -=(v: SVector) {
        this := avlsvl.sum(variable2vector(this), avlsvl.neg(v))
      }
    }

    implicit def variable2vector(v: SVariable): SVector = {
      if(context == null) throw new IRValidationException()
      SVectorRead(context, v.idx)
    }

    class SVHackImpl(val t: SVector) {
      def +(u: SVector) = avlsvl.sum(t, u)
      def -(u: SVector) = avlsvl.sum(t, avlsvl.neg(u))
      def unary_-() = avlsvl.neg(t)
      def ++(u: SVector) = avlsvl.cat(t, u)
      def apply(at: IRPoly, size: IRPoly) = avlsvl.slice(t, at, size)
      def *(u: SVector) = SVectorMpy(t, u)
      def /(u: SVector) = SVectorDiv(t, u)
      def *(d: Double) = SVectorScaleConstant(t, d)
    }

    implicit def sv2svhackimpl(t: SVector) = new SVHackImpl(t)
    implicit def svar2svhackimpl(t: SVariable) = new SVHackImpl(variable2vector(t))

    class SALMHackImpl(val t: Almap) {
      def *(u: SVariable) = t * variable2vector(u)
    }

    implicit def almap2salmhackimpl(t: Almap) = new SALMHackImpl(t)

    def converge(condition: SVector)(body: =>Unit) {
      if(condition.size != IRPoly.const(1, arity)) throw new IRValidationException()
      val curcode: Seq[SolverInstr] = code
      code = Seq()
      body
      code = curcode ++ Seq(SolverInstrConverge(context, condition, code))
    }

    def norm2(arg: SVector) = SVectorNorm2(arg)
    def sqrt(arg: SVector) = SVectorSqrt(arg)
    def dot(arg1: SVector, arg2: SVector) = SVectorDot(arg1, arg2)

    class ConeHackImpl(val c: Cone) {
      def project(t: SVector): SVector = SVectorProjCone(t, c)
    }

    def hcat(as: Almap*): Almap = {
      if(as.length == 0) {
        throw new IRValidationException()
      }
      else {
        as.drop(1).foldLeft(as(0))((b,a) => AlmapHCat(b, a))
      }
    }
    def vcat(as: Almap*): Almap = {
      if(as.length == 0) {
        throw new IRValidationException()
      }
      else {
        as.drop(1).foldLeft(as(0))((b,a) => AlmapVCat(b, a))
      }
    }
    def zeros(d: IRPoly, c: IRPoly) = AlmapZero(context.input, d, c)
    def eye(d: IRPoly) = AlmapIdentity(context.input, d)

    def zerocone(d: IRPoly): Cone = ConeFor(d, ConeZero(arity + 1))
    def freecone(d: IRPoly): Cone = ConeFor(d, ConeFree(arity + 1))

    def cat(as: Cone*): Cone = {
      if(as.length == 0) {
        throw new IRValidationException()
      }
      else {
        as.drop(1).foldLeft(as(0))((b,a) => ConeProduct(b, a))
      }
    }

    implicit def cone2conehackimpl(c: Cone) = new ConeHackImpl(c)
  }

  trait SGGen extends SGCode {
    self: Variables with Code =>

    val solver = Solver(inputSize, variables, code)
  }
}
*/

/*
trait SolverGenOps {
  self: SolverGenBase => 

  val context: SolverContext = null

  val A: Almap = problem.affineAlmap
  val b: SVector = problem.affineOffset.translate
  val F: Almap = problem.conicAlmap
  val g: SVector = problem.conicOffset.translate
  val c: SVector = problem.objective.translate
  val cone: Cone = problem.conicCone

  /*
  implicit object AVectorLikeSVectorLocal extends AVectorLike[SVector] {
    val arity: Int = context.arity
    def size(arg: SVector): IRPoly = arg.size
    def zero(size: IRPoly): SVector = SVectorZero(context, size)
    def one: SVector = SVectorOne(context)
    def add(arg1: SVector, arg2: SVector): SVector = SVectorAdd(arg1, arg2)
    def addfor(len: IRPoly, arg: SVector): SVector = SVectorAddFor(len, arg)
    def neg(arg: SVector): SVector = SVectorNeg(arg)
    def scaleinput(arg: SVector, scale: IRPoly): SVector = SVectorScaleInput(arg, scale)
    def scaleconstant(arg: SVector, scale: Double): SVector = SVectorScaleConstant(arg, scale)
    def cat(arg1: SVector, arg2: SVector): SVector = SVectorCat(arg1, arg2)
    def catfor(len: IRPoly, arg: SVector): SVector = SVectorCatFor(len, arg)
    def slice(arg: SVector, at: IRPoly, size: IRPoly): SVector = SVectorSlice(arg, at, size)

    def arityOp(op: ArityOp): AVectorLike[SVector] = AVectorLikeSVector(context.arityOp(op))
  }
  import AVectorLikeSVectorLocal._
  */

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
*/
