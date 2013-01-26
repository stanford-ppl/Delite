package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

/*

case class Expr(val vexity: Signum, val sign: Signum, val almap: Almap, val offset: AVector) extends HasArity[Expr] {
  val arity: Int = almap.arity
  val size: IRPoly = offset.size

  def arityOp(op: ArityOp): Expr = Expr(vexity, sign, almap.arityOp(op), offset.arityOp(op))

  if (almap.codomain != offset.size) throw new IRValidationException()

  def +(x: Expr): Expr = Expr(
    vexity + x.vexity,
    sign + x.sign, 
    almap + x.almap,
    offset + x.offset)

  def unary_-(): Expr = Expr(-vexity, -sign, -almap, -offset)
  def -(x: Expr): Expr = this + (-x)
  
  def apply(at: IRPoly): Expr = apply(at, IRPoly.const(1, arity))
  def apply(at: IRPoly, size: IRPoly): Expr = {
    val almappfx = AlmapHCat(AlmapHCat(AlmapZero(at, size), AlmapIdentity(size)), AlmapZero(this.size - (at + size), size))
    val almapout = almappfx * almap
    val offsetout = offset(at, size)
    Expr(vexity, sign, almapout, offsetout)
  }

  def <=(x: Expr): ConicConstraint = Constraint.nonnegative(x - this)
  def >=(x: Expr): ConicConstraint = Constraint.nonnegative(this - x)

  def *(x: Expr): Expr = {
    if((size == IRPoly.const(1, arity))&&(almap.is0)) {
      Expr.scale(x, sign, offset)
    }
    else if((x.size == IRPoly.const(1, arity))&&(x.almap.is0)) {
      Expr.scale(this, x.sign, x.offset)
    }
    else {
      throw new IRValidationException()
    }
  }

  /*
  def *(c: Expr): Expr = {
    if (shape.isInstanceOf[XShapeScalar]&&shape.asInstanceOf[XShapeScalar].desc.isinput) {
      exprscale(c, this)
    }
    else if (c.shape.isInstanceOf[XShapeScalar]&&c.shape.asInstanceOf[XShapeScalar].desc.isinput) {
      exprscale(this, c)
    }
    else {
      throw new IRValidationException()
    }
  }
  */
}

object Expr {
  // an input expression
  def input(at: IRPoly, size: IRPoly, inputSize: IRPoly, varSize: IRPoly): Expr = {
    Expr(Signum.Zero, Signum.All, AlmapZero(varSize, size), AVector.input(at, size))
  }
  // a variable expression
  def variable(at: IRPoly, size: IRPoly, inputSize: IRPoly, varSize: IRPoly): Expr = {
    Expr(Signum.Zero, Signum.All, 
      AlmapHCat(AlmapHCat(AlmapZero(at, size), AlmapIdentity(size)), AlmapZero(varSize - (at + size), size)),
      AVectorZero(size))
  }
  // a zero-size expression
  def zero(inputSize: IRPoly, varSize: IRPoly): Expr = {
    if(inputSize.arity != varSize.arity) throw new IRValidationException()
    Expr(Signum.Zero, Signum.Zero, AlmapZero(varSize, IRPoly.const(0, varSize.arity)), AVectorZero(IRPoly.const(0, varSize.arity)))
  }
  // concatenate two expressions
  def cat(arg1: Expr, arg2: Expr): Expr = {
    if(arg1.arity != arg2.arity) throw new IRValidationException()
    Expr(
      arg1.vexity + arg2.vexity,
      arg1.sign + arg2.sign,
      AlmapVCat(arg1.almap, arg2.almap),
      AVectorCat(arg1.offset, arg2.offset))
  }
  // concatenate many expressions
  def catfor(len: IRPoly, arg: Expr): Expr = {
    if(len.arity + 1 != arg.arity) throw new IRValidationException()
    Expr(
      arg.vexity,
      arg.sign,
      AlmapVCatFor(len, arg.almap),
      AVectorCatFor(len, arg.offset))
  }
  // a constant scalar expression
  def const(c: Double, inputSize: IRPoly, varSize: IRPoly): Expr = {
    if(inputSize.arity != varSize.arity) throw new IRValidationException()
    Expr(Signum.Zero, Signum.sgn(c), AlmapZero(varSize, IRPoly.const(1, varSize.arity)), AVector.const(c, varSize.arity))
  }
  // sum many expressions
  def sumfor(len: IRPoly, arg: Expr): Expr = {
    if(len.arity + 1 != arg.arity) throw new IRValidationException()
    Expr(
      arg.vexity,
      arg.sign,
      AlmapSumFor(len, arg.almap),
      AVectorAddFor(len, arg.offset))
  }
  // scale an expression
  def scale(x: Expr, sign: Signum, scale: AVector): Expr = {
    Expr(
      x.vexity * sign,
      x.sign * sign,
      scale.translate(AVectorLikeScale(x.almap, AVectorLikeAlmap(x.almap.domain))),
      scale.translate(AVectorLikeScale(x.offset, AVectorLikeAVector(scale.arity))))
  }
}

/*
def exprscale(x: Expr, c: Expr): Expr = {
  //Since c is an input, it is fully described by its offset property
  if (!(c.shape.isInstanceOf[XShapeScalar])) throw new IRValidationException()
  if (!(c.shape.asInstanceOf[XShapeScalar].desc.isinput)) throw new IRValidationException()
  if (!(x.shape.isInstanceOf[XShapeScalar])) throw new IRValidationException()
  val csign = c.shape.asInstanceOf[XShapeScalar].desc.sign
  val ysh = x.shape.morph((xd: XDesc) => 
    XDesc(csign * xd.vexity, csign * xd.sign, xd.isinput))
  val yalmap = AlmapProd(c.offset, x.almap)
  val yoffset = AlmapProd(c.offset, x.offset)
  Expr(ysh, yalmap, yoffset)
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
  globalArityPromote()
  val bsx: Expr = body(len.next)
  globalArityDemote()
  Expr(
    XShapeFor(len, bsx.shape),
    AlmapVCatFor(len, bsx.almap),
    AlmapVCatFor(len, bsx.offset))
}

def sum(len: Size, body: (Size) => Expr): Expr = {
  globalArityPromote()
  val bsx: Expr = body(len.next)
  globalArityDemote()
  Expr(
    bsx.shape.demote,
    AlmapSumFor(len, bsx.almap),
    AlmapSumFor(len, bsx.offset))
}

def xstruct(body: Seq[Expr]): Expr = Expr(
  XShapeStruct(globalArity, body map (x => x.shape)),
  AlmapVCat(globalInputShape, globalVarShape, body map (x => x.almap)),
  AlmapVCat(globalInputShape, ShapeScalar(globalArity), body map (x => x.offset)))
*/

  /*
  class Symbol[T >: Null <: HasArity[T]] {
    var binding: T = null
    def bind(e: T) {
      if (binding != null) throw new IRValidationException()
      binding = e
    }
  }
  
  implicit def symbol2Timpl[T >: Null <: HasArity[T]](s: Symbol[T]): T = {
    if (s.binding == null) throw new IRValidationException()
    var lsx: T = s.binding
    while(lsx.arity < globalArity) {
      lsx = lsx.promote
    }
    if (s.binding.arity > globalArity) throw new IRValidationException()
    lsx
  }
  */

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

*/
