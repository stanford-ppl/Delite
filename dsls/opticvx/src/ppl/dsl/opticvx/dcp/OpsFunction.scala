package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsFunction extends DCPOpsExpr {
  
  /*
    val square = {
      val x = cvxexpr()
      val t = cvxexpr()
      cvxfun(
        params(),
        args(scalar -> x),
        sign(positive),
        tonicity(x.sign),
        vexity(positive),
        over(scalar -> t),
        let(),
        where(
          in_secondorder_cone(cat(2*x, t-1), t+1)
        ),
        value(t)
      )
    }
  */

  /*
    cvxfun0(() => args()

    val square = {
      val n = cvxfunparam()
      val x = cvxfunexpr()
      val t = cvxfunexpr()
      cvxfun(
        params(),
        args(scalar -> x),
        sign(positive),
        tonicity(x.sign),
        vexity(positive),
        over(scalar -> t),
        let(),
        where(
          in_secondorder_cone(cat(2*x, t-1), t+1)
        ),
        value(t)
      )
    }
  */

  class CvxFunParams(val params: Seq[CvxParamSymbol])

  class CvxFunInputs(val inputs: Seq[CvxFunInputBinding])
  class CvxFunInputBinding(val argdesc: InputArgDesc, val symbol: CvxInputSymbol)

  class CvxArgs(val args: Seq[CvxArgBinding])
  class CvxArgBinding(val size: IRPoly, val symbol: CvxExprSymbol)

  class CvxSign(val sign: SignumPoly)

  class CvxTonicity(val tonicity: Seq[SignumPoly])

  class CvxVexity(val vexity: SignumPoly)

  class CvxOver(val vars: Seq[CvxOverBinding])
  class CvxOverBinding(val size: IRPoly, val symbol: CvxExprSymbol)

  class CvxLet(val exprs: Seq[CvxLetBinding])
  class CvxLetBinding(val expr: CvxExpr, val symbol: CvxExprSymbol)

  class CvxWhere(val constraints: Seq[CvxConstraint])

  trait CvxValue
  class CvxMinimize(val expr: CvxExpr) extends CvxValue
  class CvxMaximize(val expr: CvxExpr) extends CvxValue

  def params(ps: CvxParamSymbol*): CvxFunParams = new CvxFunParams(Seq(ps:_*))

  def given(bs: CvxFunInputBinding*): CvxFunInputs = new CvxFunInputs(Seq(bs:_*))

  def args(as: CvxArgBinding*): CvxArgs = new CvxArgs(Seq(as:_*))
  implicit def argbindingimpl(tpl: Tuple2[IRPoly, CvxExprSymbol]): CvxArgBinding =
    new CvxArgBinding(tpl._1, tpl._2)

  def sign(s: SignumPoly): CvxSign = new CvxSign(s)

  def tonicity(ts: SignumPoly*): CvxTonicity = new CvxTonicity(Seq(ts:_*))

  def vexity(v: SignumPoly): CvxVexity = new CvxVexity(v)

  def over(vs: CvxOverBinding*): CvxOver = new CvxOver(Seq(vs:_*))
  implicit def overbindingimpl(tpl: Tuple2[IRPoly, CvxExprSymbol]): CvxOverBinding = 
    new CvxOverBinding(tpl._1, tpl._2)

  def let(xs: CvxLetBinding*): CvxLet = new CvxLet(Seq(xs:_*))
  implicit def letbindingimpl(tpl: Tuple2[CvxExpr, CvxExprSymbol]): CvxLetBinding =
    new CvxLetBinding(tpl._1, tpl._2)

  def where(xs: CvxConstraint*): CvxWhere = new CvxWhere(Seq(xs:_*))

  def minimize(x: CvxExpr): CvxValue = {
    val v: Signum = x.fx.vexity.reduce
    if(!(v <= Signum.Positive)) {
      println(v)
      throw new IRValidationException()
    }
    new CvxMinimize(x)
  }
  def maximize(x: CvxExpr): CvxValue = {
    val v: Signum = x.fx.vexity.reduce
    if(!(v <= Signum.Negative)) {
      println(v)
      throw new IRValidationException()
    }
    new CvxMaximize(x)
  }

  def positive: SignumPoly = SignumPoly.const(Signum.Positive, globalSignumArity)
  def negative: SignumPoly = SignumPoly.const(Signum.Negative, globalSignumArity)
  def zero: SignumPoly = SignumPoly.const(Signum.Zero, globalSignumArity)
  def none: SignumPoly = SignumPoly.const(Signum.All, globalSignumArity)

  implicit def double2cvxfunexprimpl(c: Double): CvxExpr = 
    CvxExpr(Function.const(AVector.const(c, globalInputSize), globalInputSize, globalArgSize))
  implicit def int2cvxfunexprimpl(i: Int): CvxExpr = 
    double2cvxfunexprimpl(i.toDouble)

  def cvxfun(
    ts_params: =>CvxFunParams,
    ts_inputs: =>CvxFunInputs,
    ts_args: =>CvxArgs,
    ts_sign: =>CvxSign,
    ts_tonicity: =>CvxTonicity,
    ts_vexity: =>CvxVexity,
    ts_over: =>CvxOver,
    ts_let: =>CvxLet,
    ts_where: =>CvxWhere,
    ts_value: =>CvxValue
    ): Function =
  {
    // bind the parameters
    val s_params: Seq[CvxParamSymbol] = ts_params.params
    for(i <- 0 until s_params.length) {
      s_params(i).bind(IRPoly.param(i, s_params.length))
    }
    globalArity = s_params.length
    // bind the inputs
    val s_inputs: Seq[CvxFunInputBinding] = ts_inputs.inputs
    val s_inputsize = InputDesc(globalArity, s_inputs map (s => s.argdesc))
    globalInputSize = s_inputsize
    for(i <- 0 until s_inputs.length) {
      s_inputs(i).symbol.bind(CvxInput(i))
    }
    // bind the arguments
    val s_args: Seq[CvxArgBinding] = ts_args.args
    for(i <- 0 until s_args.length) {
      s_args(i).symbol.bindsign(SignumPoly.param(i, s_args.length))
    }
    // gather the dcp information for the function
    globalSignumArity = s_args.length
    val s_sign: SignumPoly = ts_sign.sign
    val s_tonicity: Seq[SignumPoly] = ts_tonicity.tonicity
    val s_vexity: SignumPoly = ts_vexity.vexity
    globalSignumArity = -1
    // get the variables
    val s_over = ts_over.vars
    val s_argsize: Seq[IRPoly] = (s_args map (x => x.size)) ++ (s_over map (x => x.size))
    globalArgSize = s_argsize
    // rebind the arguments
    for(i <- 0 until s_args.length) {
      s_args(i).symbol.releasesign()
      s_args(i).symbol.bindexpr(CvxExpr(Function.param(i, s_inputsize, s_argsize)))
    }
    // bind the variables
    for(i <- 0 until s_over.length) {
      s_over(i).symbol.bindexpr(CvxExpr(Function.param(i + s_args.length, s_inputsize, s_argsize)))
    }
    // bind the let-expressions
    val s_let = ts_let.exprs
    for(b <- s_let) {
      b.symbol.bindexpr(b.expr)
    }
    // get the constraints and value
    val s_where = ts_where.constraints
    val s_value = ts_value
    globalArity = -1
    globalArgSize = null
    // make the return value
    val tmpfxn = (s_value match {
      case x: CvxMinimize => s_where.foldLeft(x.expr.fx)((a,b) => a + b.fx)
      case x: CvxMaximize => s_where.foldLeft(x.expr.fx)((a,b) => a - b.fx)
      case _ => throw new IRValidationException()
    })
    val minfxn = s_over.foldLeft(tmpfxn)((a,b) => a.minimize_over_lastarg).simplify

    minfxn.chdcp(s_sign, s_tonicity, s_vexity)
  }

  /*
  val square = {
    val x = cvxfunexpr()
    val t = cvxfunexpr()
    cvxfun(
      params(),
      args(scalar -> x),
      sign(positive),
      tonicity(x.sign),
      vexity(positive),
      over(scalar -> t),
      let(),
      where(
        //in_secondorder_cone(cat(2*x, t-1), t+1)
      ),
      value(t)
    )
  }
  */
}