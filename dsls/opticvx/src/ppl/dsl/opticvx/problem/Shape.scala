package ppl.dsl.opticvx.problem

case class Size(val const: Int, val coeffs: Seq[Int])

sealed abstract class Shape

case class ShapeScalar extends Shape
case class ShapeFor(val size: Size, val body: Shape) extends Shape
case class ShapeStruct(val body: Seq[Shape]) extends Shape

