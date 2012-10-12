package ppl.dsl.opticvx.dcp

trait DCPShape {
  self: DCPSize =>

  sealed trait Shape
  
  case class ShapeScalar extends Shape
  case class ShapeFor(val size: Size, val body: Shape) extends Shape
  case class ShapeStruct(val body: Seq[Shape]) extends Shape
  

  sealed trait VShape {
    val xi: Shape
  }

  case class VShapeScalar(val vexity: Signum, val sign: Signum) extends VShape {
    val xi: Shape = ShapeScalar()
    if (!(vexity <= sign)) throw new DCPIRValidationException()
  }
  case class VShapeFor(val size: Size, val body: VShape) extends VShape {
    val xi: Shape = ShapeFor(size, body.xi)
  }
  case class VShapeStruct(val body: Seq[VShape]) extends VShape {
    val xi: Shape = ShapeStruct(body map ((x) => x.xi))
  }
  

  sealed trait TShape {
    val xi: Shape
  }

  case class TShapeScalar(val tonicity: Signum, val niltonicity: Signum) extends TShape {
    val xi: Shape = ShapeScalar()
    if (!(niltonicity <= tonicity)) throw new DCPIRValidationException()
  }
  case class TShapeFor(val size: Size, val body: TShape) extends VShape {
    val xi: Shape = ShapeFor(size, body.xi)
  }
  case class TShapeStruct(val body: Seq[TShape]) extends TShape {
    val xi: Shape = ShapeStruct(body map ((x) => x.xi))
  }

}
  
trait DCPShapeNames {
  self: DCPShape =>

  sealed class VSImplGen(val vexity: Signum, val sign: Signum)
  sealed class TSImplGen(val tonicity: Signum, val niltonicity: Signum)

  object novexity extends VSImplGen(Signum.All, Signum.All)
  object notonicity extends TSImplGen(Signum.All, Signum.All)

  object convex extends VSImplGen(Signum.Positive, Signum.All) {
    def positive = new VSImplGen(Signum.Positive, Signum.Positive)
    def nonnegative = positive
  }
  object concave extends VSImplGen(Signum.Negative, Signum.All) {
    def negative = new VSImplGen(Signum.Negative, Signum.Negative)
    def nonpositive = negative
  }
  object affine extends VSImplGen(Signum.Zero, Signum.All) {
    def positive = new VSImplGen(Signum.Zero, Signum.Positive)
    def negative = new VSImplGen(Signum.Zero, Signum.Negative)
    def nonnegative = positive
    def nonpositive = negative
    def zero = new VSImplGen(Signum.Zero, Signum.Zero)
  }

  sealed trait zero
  object zero extends zero

  object nondecreasing extends TSImplGen(Signum.Positive, Signum.Positive) {
    def at(zero: Zero) = new TSImplGen(Signum.All, Signum.Positive)
  }
  object nonincreasing extends TSImplGen(Signum.Negative, Signum.Negative) {
    def at(zero: Zero) = new TSImplGen(Signum.All, Signum.Negative)
  }
  val increasing = nondecreasing
  val decreasing = nonincreasing
  object constant extends TSImplGen(Signum.Zero, Signum.Zero) {
    def at(zero: Zero) = new TSImplGen(Signum.All, Signum.Zero)
  }

  implicit def vsimpl(z: VSImplGen): VShapeScalar = VShapeScalar(z.vexity, z.sign)
  implicit def tsimpl(z: TSImplGen): TShapeScalar = TShapeScalar(z.tonicity, z.niltonicity)
  
}