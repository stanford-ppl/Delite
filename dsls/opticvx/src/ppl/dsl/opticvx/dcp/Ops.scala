package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPOps {
  
  def cvxexpr(): Symbol[Expr] = new Symbol[Expr]()
  def cvxparam(): Symbol[Size] = new Symbol[Size]()

  class SolveParams(val params: Seq[ParamBinding])
  class ParamBinding(val param: Rep[Int], val symbol: Symbol[Size])

  def params(params: ParamBinding*): SolveParams = new SolveParams(Seq(params:_*))
  implicit def tuple2parambinding(tpl: Tuple2[Rep[Int], Symbol[Size]]): ParamBinding
    = new ParamBinding(tpl._1, tpl._2)
  implicit def flattuple2parambinding(tpl: Tuple2[Int, Symbol[Size]]): ParamBinding
    = new ParamBinding(unit(tpl._1), tpl._2)

  class SolveGiven(val inputs: Seq[InputBinding])
  class InputBinding(val input: InputDesc, val symbol: Symbol[Expr])

  def given(inputs: InputBinding*): SolveGiven = new SolveGiven(Seq(inputs:_*))
  implicit def tuple2inputbinding(tpl: Tuple2[InputDesc, Symbol[Expr]]): InputBinding
    = new InputBinding(tpl._1, tpl._2)

  class SolveOver(val vars: Seq[VarBinding])
  class VarBinding(val shape: Shape, val symbol: Symbol[Expr])

  def over(vars: VarBinding*): SolveOver = new SolveOver(Seq(vars:_*))
  implicit def tuple2varbinding(tpl: Tuple2[Shape, Symbol[Expr]]): VarBinding
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
    ts_opt: =>SolveOpt): Unit

}

trait DCPOpsExp extends DCPOps with BaseExp with ArrayOpsExp with NumericOpsExp {
  self: DCPShape with DCPInput with DCPExpr with DCPConstraint with DCPCone with DCPAlmap with DCPProblem =>
  
  def solve(
    ts_params: =>SolveParams,
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver, 
    ts_let: =>SolveLet, 
    ts_where: =>SolveWhere, 
    ts_opt: =>SolveOpt
  ) {
    //Bind the size params
    val s_params = ts_params
    val arity: Int = s_params.params.length
    for(i <- 0 until arity) {
      s_params.params(i).symbol.bind(Size.param(i, arity))
    }
    //Set the global arity
    globalArity = arity
    //Resolve the given inputs and problem variables
    val s_given = ts_given
    val input: Shape = ShapeStruct(arity, s_given.inputs map ((ii) => ii.input.shape))
    val s_over = ts_over
    val varshape: Shape = ShapeStruct(arity, s_over.vars map ((vv) => vv.shape))
    //Set the global input and varshapes
    globalInputShape = input
    globalVarShape = varshape
    //Bind the given inputs
    for(i <- 0 until s_given.inputs.length) {
      val ash = s_given.inputs(i).input.shape
      val ysh = ash.morph((nn) => XDesc(Signum.Zero, Signum.All, true))
      val yalmap = AlmapZero(input, varshape, ash)
      val ypart = AlmapHCat(input, ash,
        for (j <- 0 until s_given.inputs.length)
          yield if (i == j) AlmapIdentity(input, ash)
            else AlmapZero(input, s_given.inputs(j).input.shape, ash))
      s_given.inputs(i).symbol.bind(Expr(ysh, yalmap, almap_wrapinput(ypart)))
      //println("INS: " + s_given.inputs(i).symbol.binding.shape.toString + "\n\n")
    }
    // Bind the problem variables
    for (i <- 0 until s_over.vars.length) {
      val ysh = s_over.vars(i).shape.morph((nn) => XDesc(Signum.Zero, Signum.All, false))
      val yalmap = AlmapHCat(input, s_over.vars(i).shape,
        for (j <- 0 until s_over.vars.length)
          yield if (i == j) AlmapIdentity(input, s_over.vars(i).shape)
            else AlmapZero(input, s_over.vars(j).shape, s_over.vars(i).shape))
      val yoffset = AlmapZero(input, ShapeScalar(arity), s_over.vars(i).shape)
      s_over.vars(i).symbol.bind(Expr(ysh, yalmap, yoffset))
      //println("VAR: " + s_over.vars(i).symbol.binding.shape.toString + "\n\n")
    }
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
    val cone: Cone = ConeStruct(arity, conic_cstrt map ((cc) => cc.cone))
    val conicConstraint: Expr = xstruct(conic_cstrt map (cc => cc.expr))
    val affineConstraint: Expr = xstruct(affine_cstrt map (cc => cc.expr))
    // DCP-verify the objective
    val s_opt = ts_opt
    val objectiveExpr = s_opt.expr
    if (!(s_opt.expr.shape.isInstanceOf[XShapeScalar]))
      throw new IRValidationException()
    if (!(s_opt.expr.shape.asInstanceOf[XShapeScalar].desc.vexity <= Signum.Positive))
      throw new IRValidationException()
    // Reset the global arity, inputshape, and varshape
    globalArity = -1
    globalInputShape = null
    globalVarShape = null
    // Construct the problem
    val problem: Problem = Problem(
      affineConstraint.almap, affineConstraint.offset,
      conicConstraint.almap, conicConstraint.offset, cone)
  }
  
}


/*
trait DCPOpsTest extends DCPOps with BaseExp with ArrayOpsExp with RangeOpsExp {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr with DCPConstraint =>

  def main() {
    var input: Rep[Array[Double]] = null
  
    val x = cvxexpr()
    val y = cvxexpr()
    val z = cvxexpr()
    solve(
      over(scalar -> x, vector(input.length) -> y),
      let(x + x -> z),
      where(
        cfor(input.length)((n) => (y(n) <= x)),
        x >= 0
      ),
      minimize(
        sum(input.length)((n) => y(n)*input(n)) - x
      )
    )
  }
}
*/
