package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Set

trait DCPExpr {
  self: DCPShape with DCPShapeNames with DCPSize =>


  trait Expr {
    def shape: XShape
    //def vars: Set[OptVar]
    
    def +(x: Expr): Expr = {
      if (shape != x.shape) throw new DCPIRValidationException()
      shape match {
        case XShapeScalar(v, s, ii) => new ExprSum(this, x)
        case XShapeFor(sz, b) => {
          val ipb = new IntParamBound
          new ExprFor(sz, ipb, new ExprIndex(ipb, this) + new ExprIndex(ipb, x))
        }
        case _ => throw new DCPIRValidationException()
      }
    }
    def *(x: Expr): Expr = new ExprProd(this, x)
    def unary_-(): Expr = new ExprNeg(this)
    def -(x: Expr): Expr = new ExprSum(this, new ExprNeg(x))
  }
  
  // Optimization variables
  
  // class OptVar(varshape: Shape) extends Expr {
  //   def shape: XShape = XShapeScalar(Signum.Zero, Signum.All, false).dupshape(varshape)
  //   def vars: Set[OptVar] = Set(this)
  // }
  
  // Scalar arithmetic ops
  
  class ExprSum(arg1: Expr, arg2: Expr) extends Expr {
    def shape: XShape = {
      val arg1sh = arg1.shape.asInstanceOf[XShapeScalar]
      val arg2sh = arg2.shape.asInstanceOf[XShapeScalar]
      XShapeScalar(
        arg1sh.vexity + arg2sh.vexity, 
        arg1sh.sign + arg2sh.sign, 
        arg1sh.isInput && arg2sh.isInput)
    }
    //def vars: Set[OptVar] = arg1.vars union arg2.vars
    
    if (!arg1.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!arg2.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  }
  
  class ExprNeg(arg: Expr) extends Expr {
    def shape: XShape = {
      val argsh = arg.shape.asInstanceOf[XShapeScalar]
      XShapeScalar(-argsh.vexity, -argsh.sign, argsh.isInput)
    }
    //def vars: Set[OptVar] = arg.vars
    
    if (!arg.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  }
  
  class ExprProd(arg1: Expr, arg2: Expr) extends Expr {
    def shape: XShape = {
      val arg1sh = arg1.shape.asInstanceOf[XShapeScalar]
      val arg2sh = arg2.shape.asInstanceOf[XShapeScalar]
      XShapeScalar(
        arg1sh.vexity * arg2sh.sign + arg2sh.vexity * arg1sh.sign, 
        arg1sh.sign * arg2sh.sign, 
        arg1sh.isInput && arg2sh.isInput)
    }
    //def vars: Set[OptVar] = arg1.vars union arg2.vars
    
    if (!arg1.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!arg2.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!(arg1.shape.asInstanceOf[XShapeScalar].isInput || arg2.shape.asInstanceOf[XShapeScalar].isInput)) throw new DCPIRValidationException()
  }
  
  // Compound ops
  
  class ExprFor(size: Size, bound: IntParamBound, body: Expr) extends Expr {
    def shape: XShape = XShapeFor(size, body.shape)
    //def vars: Set[OptVar] = body.vars
  }
  
  class ExprIndex(at: Size, arg: Expr) extends Expr {
    def shape: XShape = arg.shape.asInstanceOf[XShapeFor].body
    //def vars: Set[OptVar] = arg.vars
    
    if (!arg.shape.isInstanceOf[XShapeFor]) throw new DCPIRValidationException()
  }
  
  // Input ops
  
  // class ExprInputScalar(arg: Exp[Float], sign: Signum) extends Expr {
  //   def shape: XShape = XShapeScalar(Signum.Zero, sign, true)
  //   def vars: Set[OptVar] = Set()
  // }
  
  // class ExprInputVector(size: Size, arg: Exp[Array[Float]], sign: Signum) extends Expr {
  //   def shape: XShape = XShapeFor(size, XShapeScalar(Signum.Zero, sign, true))
  //   def vars: Set[OptVar] = Set()
  // }
  
  
}