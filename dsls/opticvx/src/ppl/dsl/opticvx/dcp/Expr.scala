package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPExpr {
  self: DCPShape with DCPAlmap with DCPConstraint with DCPCone =>

  class Symbol[T>:Null] {
    var binding: T = null
    def bind(e: T) {
      if (binding != null) throw new DCPIRValidationException()
      binding = e
    }
  }
  
  implicit def symbol2Timpl[T>:Null](s: Symbol[T]): T = {
    if (s.binding == null) throw new DCPIRValidationException()
    s.binding
  }
  
  /*
  class SymbolParam {
    var binding: Size = null
    def bind(e: size) {
      if (binding != null) throw new DCPIRValidationException()
      binding = e
    }
  }

  class SymbolInput {
    
  }
  */

  case class Expr(val shape: XShape, val almap: Almap, val offset: Almap) extends HasArity[Expr] {
    val arity: Int = shape.arity
  
    def arityOp(op: ArityOp): Expr = Expr(shape.arityOp(op), almap.arityOp(op), offset.arityOp(op))
  
    if (shape.strip != almap.codomain) throw new DCPIRValidationException()
    if (shape.strip != offset.codomain) throw new DCPIRValidationException()
    if (offset.domain != ShapeScalar(arity)) throw new DCPIRValidationException()
    if (almap.input != offset.input) throw new DCPIRValidationException()
  
    def +(x: Expr): Expr = Expr(
      shape + x.shape, 
      AlmapSum(Seq(almap, x.almap)),
      AlmapSum(Seq(offset, x.offset)))

    def unary_-(): Expr = Expr(-shape, AlmapNeg(almap), AlmapNeg(offset))
    def -(x: Expr): Expr = this + (-x)
    
    def apply(at: Size): Expr = {
      throw new DCPIRValidationException()
    }

    def <=(x: Expr): ConicConstraint = constrain_nonnegative(x - this)
    def >=(x: Expr): ConicConstraint = constrain_nonnegative(this - x)

    def *(c: Expr): Expr = {
      throw new DCPIRValidationException()
    }
  }

  def infix_<=(y: Expr, c: Double): ConicConstraint = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    constrain_nonnegative(x - y)
  }
  def infix_>=(y: Expr, c: Double): ConicConstraint = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    constrain_nonnegative(y - x)
  }
  def infix_<=(c: Double, y: Expr): ConicConstraint = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    constrain_nonnegative(y - x)
  }
  def infix_>=(c: Double, y: Expr): ConicConstraint = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    constrain_nonnegative(x - y)
  }

  def infix_+(y: Expr, c: Double): Expr = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    y + x
  }
  def infix_+(c: Double, y: Expr): Expr = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    x + y
  }

  def infix_-(y: Expr, c: Double): Expr = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    y - x
  }
  def infix_-(c: Double, y: Expr): Expr = {
    val x: Expr = constant2expr(c, y.almap.input, y.almap.domain)
    x - y
  }

  def constant2expr(c: Double, input: Shape, domain: Shape): Expr = Expr(
      XShapeScalar(input.arity, Signum.Zero, Signum.All, true),
      AlmapZero(input, domain, ShapeScalar(input.arity)),
      AlmapScaleConstant(AlmapIdentity(input, ShapeScalar(input.arity)), c))

  def xfor(len: Size, body: (Size) => Expr): Expr = {
    val bsx: Expr = body(len.next)
    Expr(
      XShapeFor(len, bsx.shape),
      AlmapVCatFor(len, bsx.almap),
      AlmapVCatFor(len, bsx.offset))
  }

  def sum(len: Size, body: (Size) => Expr): Expr = {
    val bsx: Expr = body(len.next)
    Expr(
      bsx.shape,
      AlmapSumFor(len, bsx.almap),
      AlmapSumFor(len, bsx.offset))
  }

  def xstruct(body: Seq[Expr]): Expr = Expr(
    XShapeStruct(body map (x => x.shape)),
    AlmapVCat(body map (x => x.almap)),
    AlmapVCat(body map (x => x.offset)))
  
  /*
  trait Expr {
    val shape: XShape
    val varshape: Shape
    
//     def +(x: Expr): Expr = new ExprSum(this, x)
//     def *(x: Expr): Expr = new ExprProd(this, x)
//     def unary_-(): Expr = new ExprNeg(this)
//     def -(x: Expr): Expr = new ExprSum(this, new ExprNeg(x))
//     
//     def apply(at: Size): Expr = new ExprIndex(at, this)
//     
//     def <=(x: Expr): Constraint = new ConstraintNonNegative(x - this)
//     def >=(x: Expr): Constraint = new ConstraintNonNegative(this - x)
  }

  // Optimization variables
  
  case class ExprVar(val varshape: Shape) extends Expr {
    val shape: XShape = varshape.morph((n) => XDesc(Signum.Zero, Signum.All, false))
  }
  
  // Scalar arithmetic ops
  
  case class ExprSum(val arg1: Expr, val arg2: Expr) extends Expr {
    if (arg1.varshape != arg2.varshape) throw new DCPIRValidationException()
    if (!arg1.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!arg2.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  
    val varshape: Shape = arg1.varshape
    val shape: XShape = (arg1.shape, arg2.shape) match {
      case (XShapeScalar(vx1, sgn1, iip1), XShapeScalar(vx2, sgn2, iip2)) =>
        XShapeScalar(vx1 + vx2, sgn1 + sgn2, iip1 && iip2)
    }
  }
  
  class ExprNeg(val arg: Expr) extends Expr {
    if (!arg.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  
    val varshape: Shape = arg.varshape
    val shape: XShape = arg.shape match {
      case XShapeScalar(vx, sgn, iip) => XShapeScalar(-vx, -sgn, iip)
    }
  }
  
  class ExprProd(val arg1: Expr, val arg2: Expr) extends Expr {
    if (arg1.varshape != arg2.varshape) throw new DCPIRValidationException()
    if (!arg1.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!arg2.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  
    val varshape: Shape = arg1.varshape
    val shape: XShape = (arg1.shape, arg2.shape) match {
      case (XShapeScalar(vx1, sgn1, iip1), XShapeScalar(vx2, sgn2, iip2)) =>
        XShapeScalar(vx1 * sgn2 + vx2 * sgn1, sgn1 * sgn2, iip1 && iip2)
    }
  }
  
  // Compound ops
  
  class ExprFor(val size: Size, val bound: IntParamBound, val body: Expr) extends Expr {
    val varshape: Shape = body.varshape
    val shape: XShape = XShapeFor(size, body.shape)
  }
  
  class ExprReduce(val size: Size, val bound: IntParamBound, val body: Expr) extends Expr {
    if (!body.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  
    val varshape: Shape = body.varshape
    val shape: XShape = body.shape
  }
  
  class ExprIndex(val at: Size, val arg: Expr) extends Expr {
    if (!arg.shape.isInstanceOf[XShapeFor]) throw new DCPIRValidationException()
    
    val varshape: Shape = arg.varshape
    val shape: XShape = arg.shape.asInstanceOf[XShapeFor].body
  }
  
  // Input ops
  
  class ExprInputScalar(val arg: Exp[Double], val sign: Signum, val varshape: Shape) extends Expr {  
    val shape: XShape = XShapeScalar(Signum.Zero, sign, true)
  }
  
  class ExprInputVector(val size: Size, val arg: Exp[Array[Double]], val sign: Signum, val varshape: Shape) extends Expr {
    val shape: XShape = XShapeFor(size, XShapeScalar(Signum.Zero, sign, true))
  }
  */
}

