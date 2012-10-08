package ppl.dsl.opticvx.problem


class Problem(val nSizeInputs: Int, val dataShape: Shape, val varShape: Shape,
              val objective: Expr, val affineConstraint: Expr,
              val conicConstraint: Expr, val cone: Cone)
{
  def validate() {
    objective.verify(nSizeInputs, )
    affineConstraint.verify()
    conicConstraint.verify()
    cone.verify()
    assert (objective.shape == ShapeScalar())
    assert (cone.shape == conicConstraint.shape)
  }
}
