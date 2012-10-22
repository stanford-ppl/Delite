package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Set

trait DCPConstraint {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr =>

  trait Constraint {
    def verifydcp(): Unit
  }

  class ConstraintNonNegative(val x: Expr) extends Constraint {
    def verifydcp() {
      x.verifydcp()
      // Must be scalar and concave
      if (!x.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException
      if (!(x.shape.asInstanceOf[XShapeScalar].vexity <= Signum.Negative)) throw new DCPIRValidationException
    }
  }

  class ConstraintSecondOrderCone(val x: Expr, val z: Expr) extends Constraint {
    def verifydcp() {
      z.verifydcp()
      // x must be vector-shaped and affine
      if (!x.shape.isInstanceOf[XShapeFor]) throw new DCPIRValidationException
      if (!x.shape.asInstanceOf[XShapeFor].body.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException
      if (!(x.shape.asInstanceOf[XShapeFor].body.asInstanceOf[XShapeScalar].vexity <= Signum.Zero)) throw new DCPIRValidationException
      // z must be scalar-shaped and concave
      if (!z.shape.isInstanceOf[XShapeScalar]) throw new DCPIRValidationException
      if (!(z.shape.asInstanceOf[XShapeScalar].vexity <= Signum.Negative)) throw new DCPIRValidationException
    }
  }
  
  class ConstrainFor(size: Size, bound: IntParamBound, body: Constraint) extends Constraint {
    def verifydcp() {
      body.verifydcp()
    }
  }
}