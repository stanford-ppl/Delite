package ppl.dsl.opticvx.problem

case class Size(val const: Int, val coeffs: Seq[Int]) {
  def nIntParams: Int = coeffs.length
}

sealed trait Shape {
  val nIntParams: Int
  def xp(isInput: Boolean): IShape
}

case class ShapeScalar(val nIntParams: Int) extends Shape {
  def xp(isInput: Boolean): IShape = IShapeScalar(nIntParams, isInput)
}
case class ShapeFor(val nIntParams: Int, val size: Size, val body: Shape) extends Shape {
  if (size.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (body.nIntParams != (nIntParams + 1)) throw new ProblemIRValidationException()
  def xp(isInput: Boolean): IShape = IShapeFor(nIntParams, size, body.xp(isInput))
}
case class ShapeStruct(val nIntParams: Int, val body: Seq[Shape]) extends Shape {
  for (b <- body) {
    if (b.nIntParams != nIntParams) throw new ProblemIRValidationException()
  }
  def xp(isInput: Boolean): IShape = IShapeStruct(nIntParams, body map ((x) => x.xp(isInput)))
}

sealed trait IShape {
  val nIntParams: Int
  def xi: Shape
}

case class IShapeScalar(val nIntParams: Int, val isInput: Boolean) extends IShape {
  def xi: Shape = ShapeScalar(nIntParams)
}
case class IShapeFor(val nIntParams: Int, val size: Size, val body: IShape) extends IShape {
  if (size.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (body.nIntParams != (nIntParams + 1)) throw new ProblemIRValidationException()
  def xi: Shape = ShapeFor(nIntParams, size, body.xi)
}
case class IShapeStruct(val nIntParams: Int, val body: Seq[IShape]) extends IShape {
  for (b <- body) {
    if (b.nIntParams != nIntParams) throw new ProblemIRValidationException()
  }
  def xi: Shape = ShapeStruct(nIntParams, body map ((x) => x.xi))
}
