package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsFunction extends DCPOpsGlobal {
  
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

  class FunctionHack(fx: Function) {
    def apply(exprs: CvxFunExpr*) = CvxFunExpr(fx.compose(Seq(exprs:_*) map (x => x.fx)))
    def apply(params: IRPoly*) = new FunctionHack(fx.arityOp(ArityOp(params(0).arity, Seq(params:_*))))
  }

  implicit def functionhackimpl(fx: Function) = new FunctionHack(fx)

  private val positive_cone_ifx = Function.fromcone(ConeNonNegative(0))
  private val secondorder_cone_ifx = {
    val irn = IRPoly.param(0, 1)
    val irp0 = IRPoly.const(0, 1)
    val irp1 = IRPoly.const(1, 1)
    val fxinput = InputDesc(1, Seq())
    Function(
      fxinput,
      Seq(irn, irp1),
      SignumPoly.const(Signum.Positive, 2),
      Seq(SignumPoly.const(Signum.All, 2), SignumPoly.const(Signum.All, 2)),
      SignumPoly.const(Signum.Positive, 2),
      irp0,
      Seq(AlmapZero(fxinput, irn, irp1), AlmapZero(fxinput, irp1, irp1)),
      AlmapZero(fxinput, irp0, irp1),
      AVectorZero(fxinput, irp1),
      Seq(AlmapZero(fxinput, irn, irp0), AlmapZero(fxinput, irp1, irp0)),
      AlmapZero(fxinput, irp0, irp0),
      AVectorZero(fxinput, irp0),
      Seq(
        AlmapVCat(
          AlmapZero(fxinput, irn, irp1),
          AlmapIdentity(fxinput, irn)),
        AlmapVCat(
          AlmapIdentity(fxinput, irp1),
          AlmapZero(fxinput, irp1, irn))),
      AlmapZero(fxinput, irp0, irn + irp1),
      AVectorZero(fxinput, irn + irp1),
      ConeSecondOrder(irn)
    )
  }

  private val zero_ifx = {
    val irn = IRPoly.param(0, 1)
    val irp0 = IRPoly.const(0, 1)
    val irp1 = IRPoly.const(1, 1)
    val fxinput = InputDesc(1, Seq())
    Function(
      fxinput,
      Seq(irn),
      SignumPoly.const(Signum.Positive, 1),
      Seq(SignumPoly.const(Signum.All, 1)),
      SignumPoly.const(Signum.Positive, 1),
      irp0,
      Seq(AlmapZero(fxinput, irn, irp1)),
      AlmapZero(fxinput, irp0, irp1),
      AVectorZero(fxinput, irp1),
      Seq(AlmapIdentity(fxinput, irn)),
      AlmapZero(fxinput, irp0, irn),
      AVectorZero(fxinput, irn),
      Seq(AlmapZero(fxinput, irn, irp0)),
      AlmapZero(fxinput, irp0, irp0),
      AVectorZero(fxinput, irp0),
      ConeZero(1)
    )
  }

  case class CvxFunInput(val idx: Int)

  case class CvxFunExpr(val fx: Function) {
    def +(y: CvxFunExpr): CvxFunExpr = CvxFunExpr(fx + y.fx)
    def -(y: CvxFunExpr): CvxFunExpr = CvxFunExpr(fx - y.fx)
    def unary_-(): CvxFunExpr = CvxFunExpr(-fx)
    def size: IRPoly = fx.codomain
    def >=(y: CvxFunExpr): CvxFunConstraint =
      CvxFunConstraint(positive_cone_ifx(this - y).fx)
    def <=(y: CvxFunExpr): CvxFunConstraint = (y >= this)
    def ==(y: CvxFunExpr): CvxFunConstraint = {
      if(this.size != y.size) throw new IRValidationException()
      CvxFunConstraint(zero_ifx(this.size)(this - y).fx)
    }
    def +(c: Double): CvxFunExpr = this + double2expr(c)
    def -(c: Double): CvxFunExpr = this - double2expr(c)
    def *(c: Double): CvxFunExpr = CvxFunExpr(fx.scale(c))
    def /(c: Double): CvxFunExpr = CvxFunExpr(fx.scale(1/c))
  }

  implicit def double2expr(c: Double): CvxFunExpr =
    CvxFunExpr(Function.const(AVector.const(c, globalInputSize), globalInputSize, globalArgSize))

  def cat(x: CvxFunExpr, y: CvxFunExpr): CvxFunExpr =
    CvxFunExpr(Function.cat(x.fx, y.fx))

  def sumfor(len: IRPoly)(fx: (IRPoly)=>CvxFunExpr): CvxFunExpr = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val exfx = fx(len.next)
    globalArityDemote()
    CvxFunExpr(Function.sumfor(len, exfx.fx))
  }

  def xfor(len: IRPoly)(fx: (IRPoly)=>CvxFunExpr): CvxFunExpr = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val exfx = fx(len.next)
    globalArityDemote()
    CvxFunExpr(Function.catfor(len, exfx.fx))
  }

  def cfor(len: IRPoly)(fx: (IRPoly)=>CvxFunConstraint): CvxFunConstraint = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val cxfx = fx(len.next)
    globalArityDemote()
    CvxFunConstraint(Function.sumfor(len, cxfx.fx))
  }

  class DoubleHack(val c: Double) {
    def +(x: CvxFunExpr): CvxFunExpr = (x + c)
    def -(x: CvxFunExpr): CvxFunExpr = ((-x) + c)
    def *(x: CvxFunExpr): CvxFunExpr = (x * c)
    def /(x: CvxFunExpr): CvxFunExpr = throw new IRValidationException()

    def +(x: CvxFunExprSymbol): CvxFunExpr = (cvxfunexprsym2val(x) + c)
    def -(x: CvxFunExprSymbol): CvxFunExpr = ((-cvxfunexprsym2val(x)) + c)
    def *(x: CvxFunExprSymbol): CvxFunExpr = (cvxfunexprsym2val(x) * c)
    def /(x: CvxFunExprSymbol): CvxFunExpr = throw new IRValidationException()
  }
  implicit def double2doublehackimpl(c: Double): DoubleHack = new DoubleHack(c)
  implicit def int2doublehackimpl(c: Int): DoubleHack = new DoubleHack(c.toDouble)
  
  case class CvxFunConstraint(val fx: Function) {
    if(!(fx.isIndicator)) throw new IRValidationException()
  }

  def in_secondorder_cone(x: CvxFunExpr, z: CvxFunExpr): CvxFunConstraint = {
    if(z.size != IRPoly.const(1, x.size.arity)) throw new IRValidationException()
    CvxFunConstraint(secondorder_cone_ifx(x.size)(x, z).fx)
  }

  class CvxFunParamSymbol {
    protected[DCPOpsFunction] var boundparam: IRPoly = null
    protected[DCPOpsFunction] def bind(x: IRPoly) {
      if(boundparam != null) throw new IRValidationException()
      boundparam = x
    }
    protected[DCPOpsFunction] def release() {
      if(boundparam == null) throw new IRValidationException()
      boundparam = null
    }
  }
  class CvxFunInputSymbol {
    protected[DCPOpsFunction] var boundinput: CvxFunInput = null
    protected[DCPOpsFunction] def bind(x: CvxFunInput) {
      if(boundinput != null) throw new IRValidationException()
      boundinput = x
    }
    protected[DCPOpsFunction] def release() {
      if(boundinput == null) throw new IRValidationException()
      boundinput = null
    }
  }
  class CvxFunExprSymbol {
    protected[DCPOpsFunction] var boundexpr: CvxFunExpr = null
    protected[DCPOpsFunction] var boundsign: SignumPoly = null
    protected[DCPOpsFunction] def bindexpr(x: CvxFunExpr) {
      if(boundexpr != null) throw new IRValidationException()
      boundexpr = x
    }
    protected[DCPOpsFunction] def releaseexpr() {
      if(boundexpr == null) throw new IRValidationException()
      boundexpr = null
    }
    protected[DCPOpsFunction] def bindsign(x: SignumPoly) {
      if(boundsign != null) throw new IRValidationException()
      boundsign = x
    }
    protected[DCPOpsFunction] def releasesign() {
      if(boundsign == null) throw new IRValidationException()
      boundsign = null
    }
    def sign: SignumPoly = {
      if(boundsign == null) throw new IRValidationException()
      boundsign
    }
  }

  implicit def cvxfunparamsym2val(sym: CvxFunParamSymbol): IRPoly = {
    if(sym.boundparam == null) throw new IRValidationException()
    sym.boundparam.promoteTo(globalArity)
  }
  implicit def cvxfuninputsym2val(sym: CvxFunInputSymbol): CvxFunInput = {
    if(sym.boundinput == null) throw new IRValidationException()
    sym.boundinput
  }
  implicit def cvxfunexprsym2val(sym: CvxFunExprSymbol): CvxFunExpr = {
    if(sym.boundexpr == null) throw new IRValidationException()
    CvxFunExpr(sym.boundexpr.fx.promoteTo(globalArity))
  }

  def cvxparam(): CvxFunParamSymbol = new CvxFunParamSymbol
  def cvxexpr(): CvxFunExprSymbol = new CvxFunExprSymbol

  class CvxFunParams(val params: Seq[CvxFunParamSymbol])

  class CvxFunInputs(val inputs: Seq[CvxFunInputBinding])
  class CvxFunInputBinding(val argdesc: InputArgDesc, val symbol: CvxFunInputSymbol)

  class CvxFunArgs(val args: Seq[CvxFunArgBinding])
  class CvxFunArgBinding(val size: IRPoly, val symbol: CvxFunExprSymbol)

  class CvxFunSign(val sign: SignumPoly)

  class CvxFunTonicity(val tonicity: Seq[SignumPoly])

  class CvxFunVexity(val vexity: SignumPoly)

  class CvxFunOver(val vars: Seq[CvxFunOverBinding])
  class CvxFunOverBinding(val size: IRPoly, val symbol: CvxFunExprSymbol)

  class CvxFunLet(val exprs: Seq[CvxFunLetBinding])
  class CvxFunLetBinding(val expr: CvxFunExpr, val symbol: CvxFunExprSymbol)

  class CvxFunWhere(val constraints: Seq[CvxFunConstraint])

  trait CvxFunValue
  class CvxFunMinimize(val expr: CvxFunExpr) extends CvxFunValue
  class CvxFunMaximize(val expr: CvxFunExpr) extends CvxFunValue

  def params(ps: CvxFunParamSymbol*): CvxFunParams = new CvxFunParams(Seq(ps:_*))

  def args(as: CvxFunArgBinding*): CvxFunArgs = new CvxFunArgs(Seq(as:_*))
  implicit def argbindingimpl(tpl: Tuple2[IRPoly, CvxFunExprSymbol]): CvxFunArgBinding =
    new CvxFunArgBinding(tpl._1, tpl._2)

  def sign(s: SignumPoly): CvxFunSign = new CvxFunSign(s)

  def tonicity(ts: SignumPoly*): CvxFunTonicity = new CvxFunTonicity(Seq(ts:_*))

  def vexity(v: SignumPoly): CvxFunVexity = new CvxFunVexity(v)

  def over(vs: CvxFunOverBinding*): CvxFunOver = new CvxFunOver(Seq(vs:_*))
  implicit def overbindingimpl(tpl: Tuple2[IRPoly, CvxFunExprSymbol]): CvxFunOverBinding = 
    new CvxFunOverBinding(tpl._1, tpl._2)

  def let(xs: CvxFunLetBinding*): CvxFunLet = new CvxFunLet(Seq(xs:_*))
  implicit def letbindingimpl(tpl: Tuple2[CvxFunExpr, CvxFunExprSymbol]): CvxFunLetBinding =
    new CvxFunLetBinding(tpl._1, tpl._2)

  def where(xs: CvxFunConstraint*): CvxFunWhere = new CvxFunWhere(Seq(xs:_*))

  def minimize(x: CvxFunExpr): CvxFunValue = new CvxFunMinimize(x)
  def maximize(x: CvxFunExpr): CvxFunValue = new CvxFunMaximize(x)

  def positive: SignumPoly = SignumPoly.const(Signum.Positive, globalSignumArity)
  def negative: SignumPoly = SignumPoly.const(Signum.Negative, globalSignumArity)
  def zero: SignumPoly = SignumPoly.const(Signum.Zero, globalSignumArity)
  def none: SignumPoly = SignumPoly.const(Signum.All, globalSignumArity)

  implicit def double2cvxfunexprimpl(c: Double): CvxFunExpr = 
    CvxFunExpr(Function.const(AVector.const(c, globalInputSize), globalInputSize, globalArgSize))
  implicit def int2cvxfunexprimpl(i: Int): CvxFunExpr = 
    double2cvxfunexprimpl(i.toDouble)

  def cvxfun(
    ts_params: =>CvxFunParams,
    ts_inputs: =>CvxFunInputs,
    ts_args: =>CvxFunArgs,
    ts_sign: =>CvxFunSign,
    ts_tonicity: =>CvxFunTonicity,
    ts_vexity: =>CvxFunVexity,
    ts_over: =>CvxFunOver,
    ts_let: =>CvxFunLet,
    ts_where: =>CvxFunWhere,
    ts_value: =>CvxFunValue
    ): Function =
  {
    // bind the parameters
    val s_params: Seq[CvxFunParamSymbol] = ts_params.params
    for(i <- 0 until s_params.length) {
      s_params(i).bind(IRPoly.param(i, s_params.length))
    }
    globalArity = s_params.length
    // bind the inputs
    val s_inputs: Seq[CvxFunInputBinding] = ts_inputs.inputs
    val s_inputsize = InputDesc(globalArity, s_inputs map (s => s.argdesc))
    globalInputSize = s_inputsize
    for(i <- 0 until s_inputs.length) {
      s_inputs(i).symbol.bind(CvxFunInput(i))
    }
    // bind the arguments
    val s_args: Seq[CvxFunArgBinding] = ts_args.args
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
      s_args(i).symbol.bindexpr(CvxFunExpr(Function.param(i, s_inputsize, s_argsize)))
    }
    // bind the variables
    for(i <- 0 until s_over.length) {
      s_over(i).symbol.bindexpr(CvxFunExpr(Function.param(i + s_args.length, s_inputsize, s_argsize)))
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
    val tmpfxn = s_value match {
      case x: CvxFunMinimize => s_where.foldLeft(x.expr.fx)((a,b) => a + b.fx)
      case x: CvxFunMaximize => s_where.foldLeft(x.expr.fx)((a,b) => a - b.fx)
      case _ => throw new IRValidationException()
    }
    val minfxn = s_over.foldLeft(tmpfxn)((a,b) => a.minimize_over_lastarg)
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