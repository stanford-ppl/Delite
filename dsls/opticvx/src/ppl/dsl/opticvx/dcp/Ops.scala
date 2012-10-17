package ppl.dsl.opticvx.dcp

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp, ArrayOpsExp}

trait DCPOps extends Base {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr =>

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

  class Symbol

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
  class Constraint

  def where(constraints: Constraint*): SolveWhere = new SolveWhere(constraints)

  class SolveOpt(expr: Expr)

  def minimize(expr: Expr): SolveOpt = new SolveOpt(expr)
  def maximize(expr: Expr): SolveOpt = new SolveOpt(-expr)
}

trait DCPOpsExp extends DCPOps with BaseExp with ArrayOpsExp {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr =>

  def solve(s_over: SolveOver, s_let: SolveLet, s_where: SolveWhere, s_opt: SolveOpt) {
    
  }
}
