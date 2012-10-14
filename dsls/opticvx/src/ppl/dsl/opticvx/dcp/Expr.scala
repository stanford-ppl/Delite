package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Set

trait DCPExpr {
  self: DCPShape with DCPShapeNames with DCPSize =>


  trait Expr {
    def shape: XShape
    def vars: Set[OptVar]
  }
  
  // Optimization variables
  
  class OptVar(varshape: Shape) extends Expr {
    def shape: XShape = XShapeScalar(Signum.Zero, Signum.All, false).dupshape(varshape)
    def vars: Set[OptVar] = Set(this)
  }
  
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
    def vars: Set[OptVar] = arg1.vars union arg2.vars
    
    if (!arg1.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!arg2.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
  }
  
  class ExprNeg(arg: Expr) extends Expr {
    def shape: XShape = {
      val argsh = arg.shape.asInstanceOf[XShapeScalar]
      XShapeScalar(-argsh.vexity, -argsh.sign, argsh.isInput)
    }
    def vars: Set[OptVar] = arg.vars
    
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
    def vars: Set[OptVar] = arg1.vars union arg2.vars
    
    if (!arg1.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!arg2.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException()
    if (!(arg1.shape.asInstanceOf[XShapeScalar].isInput || arg2.shape.asInstanceOf[XShapeScalar].isInput)) throw new DCPIRValidationException()
  }
  
  
  
}