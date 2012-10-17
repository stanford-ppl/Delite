package ppl.dsl.opticvx.dcp

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp, ArrayOpsExp}

trait DCPOps extends Base {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr with DCPConstraint =>

  /*
  val x = cvxexpr()
  val y = cvxexpr()
  val z = cvxexpr()
  solve(
    over(scalar -> x, vector(input.length) -> y),
    let(2*x -> z),
    where(
      cfor(0 until input.length)((n) => (y(n) <= x)),
      x >= 0
    ),
    minimize(
      sum(0 until input.length)((n) => y(n)*input(n)) - x
    )
  )
  */

  def cvxexpr(): Symbol = new Symbol()
  def solve(s_over: SolveOver, s_let: SolveLet, s_where: SolveWhere, s_opt: SolveOpt): Unit

  class SolveOver(val vars: Seq[VarBinding])
  class VarBinding(val shape: Shape, val symbol: Symbol)

  def over(vars: VarBinding*): SolveOver = new SolveOver(vars)
  implicit def tuple2varbinding(tpl: Tuple2[Shape, Symbol]): VarBinding = new VarBinding(tpl._1, tpl._2)

  class SolveLet(val exprs: Seq[ExprBinding])
  class ExprBinding(val expr: Expr, val symbol: Symbol)

  def let(exprs: ExprBinding*): SolveLet = new SolveLet(exprs)
  implicit def tuple2exprbinding(tpl: Tuple2[Expr, Symbol]): ExprBinding = new ExprBinding(tpl._1, tpl._2)

  class SolveWhere(val constraints: Seq[Constraint])

  def where(constraints: Constraint*): SolveWhere = new SolveWhere(constraints)

  class SolveOpt(val expr: Expr)

  def minimize(expr: Expr): SolveOpt = new SolveOpt(expr)
  def maximize(expr: Expr): SolveOpt = new SolveOpt(-expr)

  class BoundInt
  def cfor(range: Rep[Range])(body: (BoundInt) => Constraint): Constraint
  def sum(range: Rep[Range])(body: (BoundInt) => Expr): Expr
  def xfor(range: Rep[Range])(body: (BoundInt) => Expr): Expr
}

trait DCPOpsExp extends DCPOps with BaseExp with ArrayOpsExp {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr with DCPConstraint =>

  def solve(s_over: SolveOver, s_let: SolveLet, s_where: SolveWhere, s_opt: SolveOpt) {
    // Bind the problem variables
    for (v <- s_over.vars) {
      v.symbol.bind(XShapeScalar(Signum.Zero, Signum.All, false).dupshape(v.shape))
    }
    // Bind the expression symbols
    for (x <- s_let.exprs) {
      x.expr.verifydcp()
      x.symbol.bind(x.expr.shape)
    }
    // DCP-verify the constraints
    for (c <- s_where.constraints) {
      c.verifydcp()
    }
    // DCP-verify the objective
    s_opt.expr.verifydcp()
    if (!s_opt.expr.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException
    if (!(s_opt.expr.shape.asInstanceOf[XShapeScalar].vexity <= Signum.Positive)) throw new DCPIRValidationException
    // Somehow convert this problem into the Problem IR and create a Problem IR container IR node.
  }
}
