package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Set
import scala.collection.immutable.Seq

trait DCPCone {
  self: DCPShape =>

  trait Cone extends HasArity[Cone] {
    val shape: Shape
    def dcpvalidate(xsh: XShape): Unit
  }
  
  //The trivial scalar cone (the only proper cone over R)
  case class ConeNonNegative(val arity: Int) extends Cone {
    val shape: Shape = ShapeScalar(arity)
    
    def dcpvalidate(xsh: XShape) {
      if (xsh.arity != arity) throw new DCPIRValidationException()
      if (!(xsh.isInstanceOf[XShapeScalar])) throw new DCPIRValidationException()
      val xd = xsh.asInstanceOf[XShapeScalar].desc
      if (!(xd.vexity <= Signum.Negative)) throw new DCPIRValidationException()
    }
    
    def arityOp(op: ArityOp): Cone = op match {
      case ArityOpRemoveParam(idx) => {
        if ((arity == 0)||(idx >= arity)) throw new DCPIRValidationException()
        ConeNonNegative(arity - 1)
      }
      case ArityOpAddParam(idx) => ConeNonNegative(arity + 1)
      case ArityOpSubstituteAt(idx, size) => {
        if ((arity == 0)||(idx >= arity)) throw new DCPIRValidationException()
        ConeNonNegative(arity - 1)
      }
    }

    if (shape.arity != arity) throw new DCPIRValidationException()
  }
  
  //Second order cone
  case class ConeSecondOrder(val size: Size) extends Cone {
    val arity: Int = size.arity
    val shape: Shape = ShapeStruct(
      arity, 
      Seq[Shape](
        ShapeScalar(size.arity),
        ShapeFor(
          size, 
          ShapeScalar(size.arity + 1))))
    
    def dcpvalidate(xsh: XShape) {
      if (xsh.arity != arity) throw new DCPIRValidationException()
      xsh match {
        case ShapeStructWith(
          ars, 
          Seq(
            ShapeScalarWith(arx, dx),
            ShapeForWith(szz, 
              ShapeScalarWith(arz, dz)))) => {
                if (!(dx.vexity <= Signum.Negative)) throw new DCPIRValidationException()
                if (!(dz.vexity <= Signum.Zero)) throw new DCPIRValidationException()
              }
        case _ =>
          throw new DCPIRValidationException()
      }
    }
      
    def arityOp(op: ArityOp): Cone = ConeSecondOrder(size.arityOp(op))
    
    if (shape.arity != arity) throw new DCPIRValidationException()
  }

  //For-loop product of cones
  case class ConeFor(val size: Size, val body: Cone) extends Cone {
    val arity: Int = size.arity
    val shape: Shape = ShapeFor(size, body.shape)
    
    def arityOp(op: ArityOp): Cone = ConeFor(size.arityOp(op), body.arityOp(op))
    
    def dcpvalidate(xsh: XShape) {
      if (xsh.arity != arity) throw new DCPIRValidationException()
      if (!(xsh.isInstanceOf[XShapeFor])) throw new DCPIRValidationException()
      val x = xsh.asInstanceOf[XShapeFor]
      if (x.size != size) throw new DCPIRValidationException()
      body.dcpvalidate(x.body)
    }
    
    if (body.arity != (arity + 1)) throw new DCPIRValidationException()
    
    if (shape.arity != arity) throw new DCPIRValidationException()
  }

  //Cartesian-product of cones
  case class ConeStruct(val arity: Int, val body: Seq[Cone]) extends Cone {
    val shape: Shape = ShapeStruct(arity, body map ((x) => x.shape))
    
    def arityOp(op: ArityOp): Cone = ConeStruct(op(arity), body map ((b) => b.arityOp(op)))
    
    def dcpvalidate(xsh: XShape) {
      if (xsh.arity != arity) throw new DCPIRValidationException()
      if (!(xsh.isInstanceOf[XShapeStruct])) throw new DCPIRValidationException()
      val xb = xsh.asInstanceOf[XShapeStruct].body
      if (!(xb.length == body.length)) throw new DCPIRValidationException()
      for (i <- 0 until body.length) {
        body(i).dcpvalidate(xb(i))
      }
    }
    
    for (b <- body) {
      if (b.arity != arity) throw new DCPIRValidationException()
    }
    
    if (shape.arity != arity) throw new DCPIRValidationException()
  }

}
