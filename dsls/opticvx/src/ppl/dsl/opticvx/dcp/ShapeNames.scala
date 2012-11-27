package ppl.dsl.opticvx.dcp

/*
trait DCPShapeNames {
  self: DCPShape =>

  def scalar: Shape = {
    if (globalArity == -1) throw new DCPIRValidationException()
    ShapeScalar(globalArity)
  }
  def vector(len: Size): Shape = {
    if (len.arity != globalArity) throw new DCPIRValidationException()
    ShapeFor(len, ShapeScalar(len.arity + 1))
  }
}
*/

/*
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
    
    def dupshape(sh: Shape): VShape = sh match {
      case ShapeScalar() => this
      case ShapeFor(size, body) => VShapeFor(size, dupshape(body))
      case ShapeStruct(body) => VShapeStruct(body map ((x) => dupshape(x)))
    }
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
    
    def dupshape(sh: Shape): TShape = sh match {
      case ShapeScalar() => this
      case ShapeFor(size, body) => TShapeFor(size, dupshape(body))
      case ShapeStruct(body) => TShapeStruct(body map ((x) => dupshape(x)))
    }
  }
  case class TShapeFor(val size: Size, val body: TShape) extends TShape {
    val xi: Shape = ShapeFor(size, body.xi)
  }
  case class TShapeStruct(val body: Seq[TShape]) extends TShape {
    val xi: Shape = ShapeStruct(body map ((x) => x.xi))
  }

  
  sealed trait XShape {
    val xi: Shape
    val xv: VShape
    def +(s: XShape): XShape = {
      if (xi != s.xi) throw new DCPIRValidationException()
      (this, s) match {
        case (XShapeScalar(vx1, sgn1, iip1), XShapeScalar(vx2, sgn2, iip2)) =>
          XShapeScalar(vx1 + vx2, sgn1 + sgn2, iip1 && iip2)
        case (XShapeFor(sz1, b1), XShapeFor(sz2, b2)) =>
          XShapeFor(sz1, b1 + b2)
        case (XShapeStruct(bs1), XShapeStruct(bs2)) =>
          XShapeStruct(for (i <- 0 until bs1.length) yield bs1(i) + bs2(i))
        case _ =>
          throw new DCPIRValidationException()
      }
    }
    def unary_-(): XShape = this match {
      case XShapeScalar(vx, sgn, iip) => XShapeScalar(-vx, -sgn, iip)
      case XShapeFor(sz, b) => XShapeFor(sz, -b)
      case XShapeStruct(bs) => XShapeStruct(bs map ((b) => -b))
      case _ => throw new DCPIRValidationException()
    }
  }
  
  case class XShapeScalar(val vexity: Signum, val sign: Signum, val isInput: Boolean) extends XShape {
    val xi: Shape = ShapeScalar()
    val xv: VShape = VShapeScalar(vexity, sign)
    if (!(vexity <= sign)) throw new DCPIRValidationException()
    if (isInput && (vexity != Signum.Zero)) throw new DCPIRValidationException()
    
    def dupshape(sh: Shape): XShape = sh match {
      case ShapeScalar() => this
      case ShapeFor(size, body) => XShapeFor(size, dupshape(body))
      case ShapeStruct(body) => XShapeStruct(body map ((x) => dupshape(x)))
    }
  }
  case class XShapeFor(val size: Size, val body: XShape) extends XShape {
    val xi: Shape = ShapeFor(size, body.xi)
    val xv: VShape = VShapeFor(size, body.xv)
  }
  case class XShapeStruct(val body: Seq[XShape]) extends XShape {
    val xi: Shape = ShapeStruct(body map ((x) => x.xi))
    val xv: VShape = VShapeStruct(body map ((x) => x.xv))
  }
}
  
trait DCPShapeNames {
  self: DCPShape with DCPSize with BaseExp =>

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

  sealed trait Zero
  object zero extends Zero

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

  def scalar: Shape = ShapeScalar()
  def vector(len: Exp[Int]): Shape = ShapeFor(len, ShapeScalar())
}
*/
