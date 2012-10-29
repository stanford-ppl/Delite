package ppl.dsl.opticvx.dcp

trait DCPProblem {
  self: DCPShape with DCPCone with DCPAlmap =>

  /* Maybe instead represent the affine offset as a function within LMS.
     I actually really like this idea.  Let's do this. */

  case class Problem(
    val affineConstraintAlmap: Almap,
    val affineConstraintOffset: Almap,
    val conicConstraintAlmap: Almap,
    val conicConstraintOffset: Almap,
    val conicConstraintCone: Cone) extends HasArity[Problem]
  {
    val arity: Int = affineConstraintAlmap.arity
    val variableShape: Shape = affineConstraintAlmap.domain
    val inputShape: Shape = affineConstraintAlmap.input
    
    //Verify that all expressions have the same arity
    if (affineConstraintOffset.arity != arity) throw new DCPIRValidationException()
    if (conicConstraintAlmap.arity != arity) throw new DCPIRValidationException()
    if (conicConstraintOffset.arity != arity) throw new DCPIRValidationException()
    if (conicConstraintCone.arity != arity) throw new DCPIRValidationException()

    //Verify that all expressions have the same variable shape
    if (conicConstraintAlmap.domain != variableShape) throw new DCPIRValidationException()

    //Verify that all expressions have the same input shape
    if (affineConstraintOffset.input != inputShape) throw new DCPIRValidationException()
    if (conicConstraintAlmap.input != inputShape) throw new DCPIRValidationException()
    if (conicConstraintOffset.input != inputShape) throw new DCPIRValidationException()

    //Verify that codomains agree
    if (affineConstraintAlmap.codomain != affineConstraintOffset.codomain) throw new DCPIRValidationException()
    if (conicConstraintAlmap.codomain != conicConstraintOffset.codomain) throw new DCPIRValidationException()
    if (conicConstraintAlmap.codomain != conicConstraintCone.shape) throw new DCPIRValidationException()

    //Verify that offsets have the proper domain
    if (affineConstraintOffset.domain != ShapeScalar(arity)) throw new DCPIRValidationException()
    if (conicConstraintOffset.domain != ShapeScalar(arity)) throw new DCPIRValidationException()

    def arityOp(op: ArityOp): Problem = Problem(
      affineConstraintAlmap.arityOp(op),
      affineConstraintOffset.arityOp(op),
      conicConstraintAlmap.arityOp(op),
      conicConstraintOffset.arityOp(op),
      conicConstraintCone.arityOp(op))
  }

}
