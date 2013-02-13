package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsExpr extends DCPOpsGlobal {

  class FunctionHack(fx: Function) {
    def apply(exprs: CvxExpr*) = CvxExpr(fx.compose(Seq(exprs:_*) map (x => x.fx)))
    def apply(params: IRPoly*) = new FunctionHack(fx.arityOp(ArityOp(params(0).arity, Seq(params:_*))))
  }

  implicit def functionhackimpl(fx: Function) = new FunctionHack(fx)

  private val positive_cone_ifx = Function.fromcone(ConeNonNegative(0)).chdcp(
    SignumPoly.const(Signum.Positive, 1),
    Seq(SignumPoly.const(Signum.Negative, 1)),
    SignumPoly.const(Signum.Positive, 1))

  private val secondorder_cone_ifx = {
    val irn = IRPoly.param(0, 1)
    val irp0 = IRPoly.const(0, 1)
    val irp1 = IRPoly.const(1, 1)
    val fxinput = InputDesc(1, Seq(), Seq())
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
    val fxinput = InputDesc(1, Seq(), Seq())
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
      ConeNull(1)
    )
  }

  case class CvxInput(val idx: Int)

  case class CvxExpr(val fx: Function) {
    def +(y: CvxExpr): CvxExpr = CvxExpr(fx + y.fx)
    def -(y: CvxExpr): CvxExpr = CvxExpr(fx - y.fx)
    def unary_-(): CvxExpr = CvxExpr(-fx)
    def size: IRPoly = fx.codomain
    def >=(y: CvxExpr): CvxConstraint =
      CvxConstraint(positive_cone_ifx(this - y).fx)
    def <=(y: CvxExpr): CvxConstraint = (y >= this)
    def ==(y: CvxExpr): CvxConstraint = {
      if(this.size != y.size) throw new IRValidationException()
      CvxConstraint(zero_ifx(this.size)(this - y).fx)
    }
    def +(c: Double): CvxExpr = this + double2expr(c)
    def -(c: Double): CvxExpr = this - double2expr(c)
    def *(c: Double): CvxExpr = CvxExpr(fx.scale(c))
    def /(c: Double): CvxExpr = CvxExpr(fx.scale(1/c))
    def apply(at: IRPoly): CvxExpr = apply(at, IRPoly.const(1, fx.arity))
    def apply(at: IRPoly, size: IRPoly): CvxExpr = {
      val almappfx = AlmapHCat(AlmapHCat(AlmapZero(fx.input, at, size), AlmapIdentity(fx.input, size)), AlmapZero(fx.input, this.size - (at + size), size))
      CvxExpr(Function(
        fx.input,
        fx.argSize,
        fx.sign,
        fx.tonicity,
        fx.vexity,
        fx.varSize,
        fx.valueArgAlmap map (x => almappfx * x),
        almappfx * fx.valueVarAlmap,
        fx.valueOffset(at, size),
        fx.affineArgAlmap,
        fx.affineVarAlmap,
        fx.affineOffset,
        fx.conicArgAlmap,
        fx.conicVarAlmap,
        fx.conicOffset,
        fx.conicCone))
    }
  }

  implicit def double2expr(c: Double): CvxExpr =
    CvxExpr(Function.const(AVector.const(c, globalInputSize), globalInputSize, globalArgSize))

  def cat(x: CvxExpr, y: CvxExpr): CvxExpr =
    CvxExpr(Function.cat(x.fx, y.fx))

  def sumfor(len: IRPoly)(fx: (IRPoly)=>CvxExpr): CvxExpr = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val exfx = fx(len.next)
    globalArityDemote()
    CvxExpr(Function.sumfor(len, exfx.fx))
  }

  def xfor(len: IRPoly)(fx: (IRPoly)=>CvxExpr): CvxExpr = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val exfx = fx(len.next)
    globalArityDemote()
    CvxExpr(Function.catfor(len, exfx.fx))
  }

  def cfor(len: IRPoly)(fx: (IRPoly)=>CvxConstraint): CvxConstraint = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val cxfx = fx(len.next)
    globalArityDemote()
    CvxConstraint(Function.sumfor(len, cxfx.fx))
  }

  class DoubleHack(val c: Double) {
    def +(x: CvxExpr): CvxExpr = (x + c)
    def -(x: CvxExpr): CvxExpr = ((-x) + c)
    def *(x: CvxExpr): CvxExpr = (x * c)
    def /(x: CvxExpr): CvxExpr = throw new IRValidationException()

    def +(x: CvxExprSymbol): CvxExpr = (cvxexprsym2val(x) + c)
    def -(x: CvxExprSymbol): CvxExpr = ((-cvxexprsym2val(x)) + c)
    def *(x: CvxExprSymbol): CvxExpr = (cvxexprsym2val(x) * c)
    def /(x: CvxExprSymbol): CvxExpr = throw new IRValidationException()
  }
  implicit def double2doublehackimpl(c: Double): DoubleHack = new DoubleHack(c)
  implicit def int2doublehackimpl(c: Int): DoubleHack = new DoubleHack(c.toDouble)
  
  case class CvxConstraint(val fx: Function) {
    if(!(fx.isIndicator)) throw new IRValidationException()
    if(!(fx.vexity.reduce <= Signum.Positive)) {
      println(fx.vexity.reduce)
      throw new IRValidationException()
    }
  }

  def in_secondorder_cone(x: CvxExpr, z: CvxExpr): CvxConstraint = {
    if(z.size != IRPoly.const(1, x.size.arity)) throw new IRValidationException()
    CvxConstraint(secondorder_cone_ifx(x.size)(x, z).fx)
  }

  class CvxParamSymbol {
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
  class CvxInputSymbol {
    protected[dcp] var boundinput: CvxInput = null
    protected[dcp] def bind(x: CvxInput) {
      if(boundinput != null) throw new IRValidationException()
      boundinput = x
    }
    protected[dcp] def release() {
      if(boundinput == null) throw new IRValidationException()
      boundinput = null
    }
  }
  class CvxExprSymbol {
    protected[dcp] var boundexpr: CvxExpr = null
    protected[dcp] var boundsign: SignumPoly = null
    protected[dcp] var resolution: Seq[Double] = null
    protected[dcp] def bindexpr(x: CvxExpr) {
      if(boundexpr != null) throw new IRValidationException()
      if(resolution != null) throw new IRValidationException()
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
    protected[dcp] def rset(r: Seq[Double]) {
      releaseexpr()
      if(resolution != null) throw new IRValidationException()
      resolution = r
    }
    def sign: SignumPoly = {
      if(boundsign == null) throw new IRValidationException()
      boundsign
    }
    def resolve: Seq[Double] = {
      if(resolution == null) throw new IRValidationException()
      resolution
    }
  }

  implicit def cvxparamsym2val(sym: CvxParamSymbol): IRPoly = {
    if(sym.boundparam == null) throw new IRValidationException()
    sym.boundparam.promoteTo(globalArity)
  }
  implicit def cvxinputsym2val(sym: CvxInputSymbol): CvxInput = {
    if(sym.boundinput == null) throw new IRValidationException()
    sym.boundinput
  }
  implicit def cvxexprsym2val(sym: CvxExprSymbol): CvxExpr = {
    if(sym.boundexpr == null) throw new IRValidationException()
    CvxExpr(sym.boundexpr.fx.promoteTo(globalArity))
  }

  def cvxparam(): CvxParamSymbol = new CvxParamSymbol
  def cvxinput(): CvxInputSymbol = new CvxInputSymbol
  def cvxexpr(): CvxExprSymbol = new CvxExprSymbol

}
