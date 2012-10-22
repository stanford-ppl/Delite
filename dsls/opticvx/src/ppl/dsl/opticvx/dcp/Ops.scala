package ppl.dsl.opticvx.dcp

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp, ArrayOpsExp, RangeOpsExp, NumericOps, NumericOpsExp}

trait DCPOps extends Base with NumericOps {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr with DCPConstraint =>

  def cvxexpr(): Symbol = new Symbol()
  def solve(s_over: SolveOver, s_let: SolveLet, s_where: SolveWhere, s_opt: SolveOpt): Unit

  class SolveOver(val vars: Seq[VarBinding])
  class VarBinding(val shape: Shape, val symbol: Symbol)

  def over(vars: VarBinding*): SolveOver = new SolveOver(vars)
  implicit def tuple2varbinding(tpl: Tuple2[Shape, Symbol]): VarBinding
    = new VarBinding(tpl._1, tpl._2)

  class SolveLet(val exprs: Seq[ExprBinding])
  class ExprBinding(val expr: Expr, val symbol: Symbol)

  def let(exprs: ExprBinding*): SolveLet = new SolveLet(exprs)
  implicit def tuple2exprbinding(tpl: Tuple2[Expr, Symbol]): ExprBinding
    = new ExprBinding(tpl._1, tpl._2)

  class SolveWhere(val constraints: Seq[Constraint])

  def where(constraints: Constraint*): SolveWhere = new SolveWhere(constraints)

  class SolveOpt(val expr: Expr)

  def minimize(expr: Expr): SolveOpt = new SolveOpt(expr)
  def maximize(expr: Expr): SolveOpt = new SolveOpt(-expr)

  def cfor(len: Size)(body: (IntParamBound) => Constraint): Constraint
  def sum(len: Size)(body: (IntParamBound) => Expr): Expr
  def xfor(len: Size)(body: (IntParamBound) => Expr): Expr
  
  implicit def inputscalar(c: Double): Expr = inputscalar(unit(c))
  implicit def inputscalar(c: Float): Expr = inputscalar(c.toDouble)
  implicit def inputscalar(c: Int): Expr = inputscalar(c.toDouble)
  
  implicit def inputscalar(c: Rep[Double]): Expr
  def inputvector(ar: Rep[Array[Double]]): Expr
  
  class ArrayIndexHack(ar: Rep[Array[Double]]) {
    def apply(at: Size): Expr = (inputvector(ar))(at)
  }
  
  implicit def arrayindexhackimpl(ar: Rep[Array[Double]]) = new ArrayIndexHack(ar)
}

trait DCPOpsExp extends DCPOps with BaseExp with ArrayOpsExp with NumericOpsExp {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr with DCPConstraint =>
  
  implicit def inputscalar(c: Exp[Double]): Expr = new ExprInputScalar(c, Signum.All)
  def inputvector(ar: Exp[Array[Double]]): Expr = new ExprInputVector(ar.length, ar, Signum.All)
  
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
  
  def cfor(len: Size)(body: (IntParamBound) => Constraint): Constraint = {
    val ipb = new IntParamBound
    new ConstrainFor(len, ipb, body(ipb))   
  }
  
  def sum(len: Size)(body: (IntParamBound) => Expr): Expr = {
    val ipb = new IntParamBound
    new ExprReduce(len, ipb, body(ipb))  
  }
  
  def xfor(len: Size)(body: (IntParamBound) => Expr): Expr = {
    val ipb = new IntParamBound
    new ExprFor(len, ipb, body(ipb))    
  }
}

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
