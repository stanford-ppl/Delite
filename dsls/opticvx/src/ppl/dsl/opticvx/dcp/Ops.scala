package ppl.dsl.opticvx.dcp

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp, ArrayOpsExp, RangeOpsExp, NumericOps, NumericOpsExp}

trait DCPOps extends Base with NumericOps {
  self: DCPShape with DCPExpr with DCPConstraint =>

  def cvxexpr(): Symbol[Expr] = new Symbol[Expr]()
  def cvxinput(): Symbol[InputDesc] = new Symbol[InputDesc]()
  def cvxparam(): Symbol[Size] = new Symbol[Size]()

  def solve(
    ts_params: =>SolveParams,
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver,
    ts_let: =>SolveLet,
    ts_where: =>SolveWhere,
    ts_opt: =>SolveOpt): Unit

  class SolveParams(val params: Seq[ParamBinding])
  class ParamBinding(val param: Rep[Int], val symbol: Symbol[Size])

  def params(params: ParamBinding*): SolveParams = new SolveParams(params)
  implicit def tuple2parambinding(tpl: Tuple2[Rep[Int], Symbol[Size]]): ParamBinding
    = new ParamBinding(tpl._1, tpl._2)
  implicit def flattuple2parambinding(tpl: Tuple2[Int, Symbol[Size]]): ParamBinding
    = new ParamBinding(unit(tpl._1), tpl._2)

  class SolveGiven(val inputs: Seq[InputBinding])
  class InputBinding(val input: InputDesc, val symbol: Symbol[InputDesc])

  def given(inputs: InputBinding*): SolveGiven = new SolveGiven(inputs)
  implicit def tuple2inputbinding(tpl: Tuple2[InputDesc, Symbol[InputDesc]]): InputBinding
    = new InputBinding(tpl._1, tpl._2)
    
  sealed trait InputDesc
  case class InputDescScalar(val input: Rep[Double]) extends InputDesc
  case class InputDescFor(val size: Size, val body: (Rep[Int]) => InputDesc) extends InputDesc

  implicit def dbl2inputdesc(input: Double): InputDesc = InputDescScalar(unit(input))
  implicit def repdbl2inputdesc(input: Rep[Double]): InputDesc = InputDescScalar(input)
  def ifor(size: Size)(body: (Rep[Int]) => InputDesc): InputDesc = InputDescFor(size, body)

  class SolveOver(val vars: Seq[VarBinding])
  class VarBinding(val shape: Shape, val symbol: Symbol[Expr])

  def over(vars: VarBinding*): SolveOver = new SolveOver(vars)
  implicit def tuple2varbinding(tpl: Tuple2[Shape, Symbol[Expr]]): VarBinding
    = new VarBinding(tpl._1, tpl._2)

  class SolveLet(val exprs: Seq[ExprBinding])
  class ExprBinding(val expr: Expr, val symbol: Symbol[Expr])

  def let(exprs: ExprBinding*): SolveLet = new SolveLet(exprs)
  implicit def tuple2exprbinding(tpl: Tuple2[Expr, Symbol[Expr]]): ExprBinding
    = new ExprBinding(tpl._1, tpl._2)

  class SolveWhere(val constraints: Seq[Constraint])

  def where(constraints: Constraint*): SolveWhere = new SolveWhere(constraints)

  class SolveOpt(val expr: Expr)

  def minimize(expr: Expr): SolveOpt = new SolveOpt(expr)
  //def maximize(expr: Expr): SolveOpt = new SolveOpt(-expr)

  def cfor(len: Size)(body: (Size) => Constraint): Constraint
  def sum(len: Size)(body: (Size) => Expr): Expr
  def xfor(len: Size)(body: (Size) => Expr): Expr
  
  implicit def inputscalar(c: Double): Expr = inputscalar(unit(c))
  implicit def inputscalar(c: Float): Expr = inputscalar(c.toDouble)
  implicit def inputscalar(c: Int): Expr = inputscalar(c.toDouble)
  
  implicit def inputscalar(c: Rep[Double]): Expr
  def inputvector(ar: Rep[Array[Double]]): Expr
  
  //class ArrayIndexHack(ar: Rep[Array[Double]]) {
  //  def apply(at: Size): Expr = (inputvector(ar))(at)
  //}
  
  //implicit def arrayindexhackimpl(ar: Rep[Array[Double]]) = new ArrayIndexHack(ar)
}
/*
trait DCPOpsExp extends DCPOps with BaseExp with ArrayOpsExp with NumericOpsExp {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr with DCPConstraint =>
  /*
  implicit def inputscalar(c: Exp[Double]): Expr = new ExprInputScalar(c, Signum.All)
  def inputvector(ar: Exp[Array[Double]]): Expr = new ExprInputVector(ar.length, ar, Signum.All)
  
  def solve(
    ts_params: =>SolveParams,
    ts_given: =>SolveGiven,
    ts_over: =>SolveOver, 
    ts_let: =>SolveLet, 
    ts_where: =>SolveWhere, 
    ts_opt: =>SolveOpt
  ) {
    val s_over = ts_over  
    // Bind the problem variables
    for (v <- s_over.vars) {
      v.symbol.bind(XShapeScalar(Signum.Zero, Signum.All, false).dupshape(v.shape))
    }
    val s_let = ts_let
    // Bind the expression symbols
    for (x <- s_let.exprs) {
      x.expr.verifydcp()
      x.symbol.bind(x.expr.shape)
    }
    val s_where = ts_where
    val s_opt = ts_opt
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
  */
}
*/
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