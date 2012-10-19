package ppl.dsl.opticvx.problem

class ProblemIRValidationException extends Exception

/*
case class Problem(
  val variableShape: Shape,
  val dataInputShape: Shape,
  val objective: Expr,
  val affineConstraint: Expr,
  val conicConstraint: Expr,
  val cone: Cone)
{
  val nIntParams: Int = dataInputShape.nIntParams
  
  //Verify that all expressions have the same arity
  if (dataInputShape.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (variableShape.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (objective.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (affineConstraint.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (conicConstraint.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (cone.nIntParams != nIntParams) throw new ProblemIRValidationException()
  
  //Verify that all expressions have the correct bindings
  val problemBindings = Seq[IShape](variableShape.xp(false), dataInputShape.xp(true))
  if (objective.bindings != problemBindings) throw new ProblemIRValidationException()
  if (affineConstraint.bindings != problemBindings) throw new ProblemIRValidationException()
  if (conicConstraint.bindings != problemBindings) throw new ProblemIRValidationException()
  
  //Verify that the expressions are of the correct shape
  objective.shape match {
    case IShapeScalar(nip, ii) => 
    case _ => throw new ProblemIRValidationException()
  }
  val affineConstraintSize: Size = affineConstraint.shape match {
    case IShapeFor(_, sz, IShapeScalar(_, _)) => sz
    case _ => throw new ProblemIRValidationException()
  }
  if (conicConstraint.shape.xi != cone.shape) throw new ProblemIRValidationException()
}
*/
