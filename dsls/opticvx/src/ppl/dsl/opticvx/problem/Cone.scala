package ppl.dsl.opticvx.problem

abstract class Cone {
  def shape: Shape
  def verify(): Unit
}



case class ConeFor(val size: Size, val body: Cone) extends Cone {
  def shape: Shape = ShapeFor(size, body.shape)
}
case class ConeStruct(val body: Seq[Cone]) extends Cone {
  def shape: Shape = ShapeStruct(body map ((x) => x.shape))
}
