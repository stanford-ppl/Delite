package ppl.dsl.opticvx.problem

case class Size(val const: Int, val coeffs: Seq[Int]) {
  def nIntParams: Int = coeffs.length
}

sealed abstract class Shape

case class ShapeScalar extends Shape
case class ShapeFor(val size: Size, val body: Shape) extends Shape
case class ShapeStruct(val body: Seq[Shape]) extends Shape

sealed trait IShape {
  val nIntParams: Int
}

case class IShapeScalar(val nIntParams: Int, val isInput: Boolean) extends IShape
case class IShapeFor(val nIntParams: Int, val size: Size, val body: IShape) extends IShape {
  if (size.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (body.nIntParams != (nIntParams + 1)) throw new ProblemIRValidationException()
}
case class IShapeStruct(val nIntParams: Int, val body: Seq[IShape]) extends IShape {
  for (b <- body) {
    if (b.nIntParams != nIntParams) throw new ProblemIRValidationException()
  }
}
