package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsExpr extends DCPOpsGlobal {

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
    protected[dcp] var boundparam: IRPoly = null
    protected[dcp] def bind(x: IRPoly) {
      if(boundparam != null) throw new IRValidationException()
      boundparam = x
    }
    protected[dcp] def release() {
      if(boundparam == null) throw new IRValidationException()
      boundparam = null
    }
  }
  class CvxFunInputSymbol {
    protected[dcp] var boundinput: CvxFunInput = null
    protected[dcp] def bind(x: CvxFunInput) {
      if(boundinput != null) throw new IRValidationException()
      boundinput = x
    }
    protected[dcp] def release() {
      if(boundinput == null) throw new IRValidationException()
      boundinput = null
    }
  }
  class CvxFunExprSymbol {
    protected[dcp] var boundexpr: CvxFunExpr = null
    protected[dcp] var boundsign: SignumPoly = null
    protected[dcp] def bindexpr(x: CvxFunExpr) {
      if(boundexpr != null) throw new IRValidationException()
      boundexpr = x
    }
    protected[dcp] def releaseexpr() {
      if(boundexpr == null) throw new IRValidationException()
      boundexpr = null
    }
    protected[dcp] def bindsign(x: SignumPoly) {
      if(boundsign != null) throw new IRValidationException()
      boundsign = x
    }
    protected[dcp] def releasesign() {
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

}
