package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait HasSize {
  val size: IRPoly
}

trait DCPOps extends DCPOpsGlobal {
  
  type ParamDesc
  type InputDesc <: HasSize

  def cvxexpr(): Symbol[Expr] = new Symbol[Expr]()
  def cvxparam(): Symbol[IRPoly] = new Symbol[IRPoly]()

  def scalar: IRPoly = IRPoly.const(1, globalArity)
  def vector(size: IRPoly): IRPoly = size

  implicit def int2irpoly(i: Int) = IRPoly.const(i, globalArity)
  implicit def double2expr(x: Double) = Expr.const(x, globalInputSize, globalVarSize)

  def sumfor(len: IRPoly)(fx: (IRPoly)=>Expr): Expr = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val exfx = fx(len.next)
    globalArityDemote()
    Expr.sumfor(len, exfx)
  }

  def xfor(len: IRPoly)(fx: (IRPoly)=>Expr): Expr = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val exfx = fx(len.next)
    globalArityDemote()
    Expr.catfor(len, exfx)
  }

  def cfor(len: IRPoly)(fx: (IRPoly)=>Constraint): Constraint = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val cxfx = fx(len.next)
    globalArityDemote()
    Constraint.catfor(len, cxfx)
  }

  class SolveParams(val params: Seq[ParamBinding])
  class ParamBinding(val param: ParamDesc, val symbol: Symbol[IRPoly])

  def params(params: ParamBinding*): SolveParams = new SolveParams(Seq(params:_*))
  implicit def tuple2parambinding(tpl: Tuple2[ParamDesc, Symbol[IRPoly]]): ParamBinding
    = new ParamBinding(tpl._1, tpl._2)

  class SolveGiven(val inputs: Seq[InputBinding])
  class InputBinding(val input: InputDesc, val symbol: Symbol[Expr])

  def given(inputs: InputBinding*): SolveGiven = new SolveGiven(Seq(inputs:_*))
  implicit def tuple2inputbinding(tpl: Tuple2[InputDesc, Symbol[Expr]]): InputBinding
    = new InputBinding(tpl._1, tpl._2)

  class SolveOver(val vars: Seq[VarBinding])
  class VarBinding(val size: IRPoly, val symbol: Symbol[Expr])

  def over(vars: VarBinding*): SolveOver = new SolveOver(Seq(vars:_*))
  implicit def tuple2varbinding(tpl: Tuple2[IRPoly, Symbol[Expr]]): VarBinding
    = new VarBinding(tpl._1, tpl._2)

  class SolveLet(val exprs: Seq[ExprBinding])
  class ExprBinding(val expr: Expr, val symbol: Symbol[Expr])

  def let(exprs: ExprBinding*): SolveLet = new SolveLet(Seq(exprs:_*))
  implicit def tuple2exprbinding(tpl: Tuple2[Expr, Symbol[Expr]]): ExprBinding
    = new ExprBinding(tpl._1, tpl._2)

  class SolveWhere(val constraints: Seq[Constraint])

  def where(constraints: Constraint*): SolveWhere = new SolveWhere(Seq(constraints:_*))

  class SolveOpt(val expr: Expr)

  def minimize(expr: Expr): SolveOpt = new SolveOpt(expr)
  def maximize(expr: Expr): SolveOpt = new SolveOpt(-expr)

  def solve(
    ts_params: =>SolveParams,
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver,
    ts_let: =>SolveLet,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)
  {
    //Bind the size params
    val s_params = ts_params
    val arity: Int = s_params.params.length
    for(i <- 0 until arity) {
      s_params.params(i).symbol.bind(IRPoly.param(i, arity))
    }
    //Set the global arity
    globalArity = arity
    //Resolve the given inputs and problem variables
    val s_given = ts_given
    val inputSize: IRPoly = s_given.inputs.foldLeft(IRPoly.const(0, arity))((i, s) => i + s.input.size)
    val s_over = ts_over
    val varSize: IRPoly = s_over.vars.foldLeft(IRPoly.const(0, arity))((i, s) => i + s.size)
    //Set the global input and varshapes
    globalInputSize = inputSize
    globalVarSize = varSize
    //Bind the given inputs
    var inputAt: IRPoly = IRPoly.const(0, arity)
    for(i <- 0 until s_given.inputs.length) {
      val ash = s_given.inputs(i).input.size
      s_given.inputs(i).symbol.bind(Expr.input(inputAt, ash, inputSize, varSize))
      inputAt = inputAt + ash
    }
    // Sanity check
    if(inputAt != inputSize) throw new IRValidationException()
    // Bind the problem variables
    var varAt: IRPoly = IRPoly.const(0, arity)
    for (i <- 0 until s_over.vars.length) {
      val ash = s_over.vars(i).size
      s_over.vars(i).symbol.bind(Expr.variable(varAt, ash, inputSize, varSize))
      varAt = varAt + ash
    }
    // Sanity check
    if (varAt != varSize) throw new IRValidationException()
    // Bind the expression symbols
    val s_let = ts_let
    for (x <- s_let.exprs) {
      x.symbol.bind(x.expr)
    }
    // Assemble the constraints
    val s_where = ts_where
    val conic_cstrt: Seq[ConicConstraint] = 
      s_where.constraints filter (c => c.isInstanceOf[ConicConstraint]) map (c => c.asInstanceOf[ConicConstraint])
    val affine_cstrt: Seq[AffineConstraint] = 
      s_where.constraints filter (c => c.isInstanceOf[AffineConstraint]) map (c => c.asInstanceOf[AffineConstraint])
    var cone: Cone = conic_cstrt.foldLeft(ConeZero(arity): Cone)((i, s) => ConeProduct(i, s.cone))
    val conicConstraint: Expr = conic_cstrt.foldLeft(Expr.zero(inputSize, varSize))((i, s) => Expr.cat(i, s.expr))
    val affineConstraint: Expr = affine_cstrt.foldLeft(Expr.zero(inputSize, varSize))((i, s) => Expr.cat(i, s.expr))
    // DCP-verify the objective
    val s_opt = ts_opt
    val objectiveExpr = s_opt.expr
    if(objectiveExpr.size != IRPoly.const(1, arity)) throw new IRValidationException()
    if(!(objectiveExpr.vexity <= Signum.Positive)) throw new IRValidationException()
    // Convert the objective expression to a vector
    val objectiveVector = objectiveExpr.almap.T.mmpy(AVectorOne(arity): AVector)(AVectorLikeAVector(arity))
    // Reset the global arity, inputshape, and varshape
    globalArity = -1
    globalInputSize = null
    globalVarSize = null
    // Construct the problem
    val problem: Problem = Problem(
      inputSize,
      objectiveVector,
      affineConstraint.almap, affineConstraint.offset,
      conicConstraint.almap, conicConstraint.offset, cone)

    postsolve(problem, s_params.params map (p => p.param), s_given.inputs map (i => i.input))
  }

  def postsolve(problem: Problem, params: Seq[ParamDesc], inputs: Seq[InputDesc]) { }

  object SolveImplicit1
  def solve(
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver,
    ts_let: =>SolveLet,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit1.type): Unit = solve(params(), ts_given, ts_over, ts_let, ts_where, ts_opt)

  object SolveImplicit2
  def solve(
    ts_params: =>SolveParams,
    ts_over: =>SolveOver,
    ts_let: =>SolveLet,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit2.type): Unit = solve(ts_params, given(), ts_over, ts_let, ts_where, ts_opt)

  object SolveImplicit3
  def solve(
    ts_over: =>SolveOver,
    ts_let: =>SolveLet,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit3.type): Unit = solve(params(), given(), ts_over, ts_let, ts_where, ts_opt)

  object SolveImplicit4
  def solve(
    ts_params: =>SolveParams,
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit4.type): Unit = solve(ts_params, ts_given, ts_over, let(), ts_where, ts_opt)

  object SolveImplicit5
  def solve(
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit5.type): Unit = solve(params(), ts_given, ts_over, let(), ts_where, ts_opt)

  object SolveImplicit6
  def solve(
    ts_params: =>SolveParams,
    ts_over: =>SolveOver,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit6.type): Unit = solve(ts_params, given(), ts_over, let(), ts_where, ts_opt)

  object SolveImplicit7
  def solve(
    ts_over: =>SolveOver,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt)(implicit e: SolveImplicit7.type): Unit = solve(params(), given(), ts_over, let(), ts_where, ts_opt)

}

