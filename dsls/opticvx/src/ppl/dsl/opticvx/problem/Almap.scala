// Almap = Abstract Linear MAP
// This represents a linear map as a composition of a set of linear mapping primitives

package ppl.dsl.opticvx.problem

import scala.collection.immutable.Seq

trait Almap extends HasArity[Almap] {
  //The shape of the input parameters used by this map
  val input: Shape
  //The domain and codomain of this map
  val domain: Shape
  val codomain: Shape

  //Constraints that all the shape properties must share the map's arity
  if (input.arity != arity) throw new ProblemIRValidationException()
  if (domain.arity != arity) throw new ProblemIRValidationException()
  if (codomain.arity != arity) throw new ProblemIRValidationException()

  //Matrix multiply
  def mmpy(x: Almap): Almap
  
  //The transpose
  def T: Almap
}

object AlmapUtil {
  def sum(x: Almap, y: Almap): Almap = {
    if (x.arity != y.arity) throw new ProblemIRValidationException()
    if (x.input != y.input) throw new ProblemIRValidationException()
    if (x.domain != y.domain) throw new ProblemIRValidationException()
    if (x.codomain != y.codomain) throw new ProblemIRValidationException()
    x.codomain match {
      case ShapeScalar(ar) => AlfSum(x, y)
      case ShapeFor(sz, b) => 
        AlmapFor(sz, sum(
          AlmapIndex(Size.param(x.arity, x.arity + 1), x.promote),
          AlmapIndex(Size.param(x.arity, x.arity + 1), y.promote)))
      case ShapeStruct(bs) =>
        AlmapStruct(for (i <- 0 until bs.length) yield
          sum(AlmapAccess(i, x), AlmapAccess(i, y)))
    }
  }
  
  def neg(x: Almap): Almap = {
    x.codomain match {
      case ShapeScalar(ar) => AlfNeg(x)
      case ShapeFor(sz, b) =>
        AlmapFor(sz, neg(AlmapIndex(Size.param(x.arity, x.arity + 1), x.promote)))
      case ShapeStruct(bs) =>
        AlmapStruct(for (i <- 0 until bs.length) yield neg(AlmapAccess(i, x)))
    }
  }
  
  def scale(x: Almap, c: Almap): Almap = {
    if (x.arity != c.arity) throw new ProblemIRValidationException()
    if (x.input != c.input) throw new ProblemIRValidationException()
    if (x.input != c.domain) throw new ProblemIRValidationException()
    if (!(c.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()
    x.codomain match {
      case ShapeScalar(ar) => AlfScale(x, c)
      case ShapeFor(sz, b) =>
        AlmapFor(sz, scale(AlmapIndex(Size.param(x.arity, x.arity + 1), x.promote), c))
      case ShapeStruct(bs) =>
        AlmapStruct(for (i <- 0 until bs.length) yield scale(AlmapAccess(i, x), c))
    }
  }
  
  def scaleconstant(x: Almap, c: Float): Almap = {
    x.codomain match {
      case ShapeScalar(ar) => AlfScaleConstant(x, c)
      case ShapeFor(sz, b) =>
        AlmapFor(sz, scaleconstant(AlmapIndex(Size.param(x.arity, x.arity + 1), x.promote), c))
      case ShapeStruct(bs) =>
        AlmapStruct(for (i <- 0 until bs.length) yield scaleconstant(AlmapAccess(i, x), c))
    }
  }
  
  def reduce(s: Size, x: Almap): Almap = {
    if (x.arity != (s.arity + 1)) throw new ProblemIRValidationException()
    x.codomain match {
      case ShapeScalar(ar) => AlfReduce(s, x)
      case ShapeFor(sz, b) =>
        AlmapFor(sz, reduce(s, 
          AlmapIndex(Size.param(s.arity, s.arity + 1), x.addParam(s.arity))))
      case ShapeStruct(bs) =>
        AlmapStruct(for (i <- 0 until bs.length) yield reduce(s, AlmapAccess(i, x)))
    }
  }
}


//The identity map over a space
case class AlmapIdentity(val domain: Shape, val input: Shape) extends Almap {
  val arity: Int = domain.arity
  val codomain: Shape = domain
  
  def arityOp(op: ArityOp): Almap = AlmapIdentity(domain.arityOp(op), input.arityOp(op))

  def mmpy(x: Almap): Almap = {
    if(arity != x.arity) throw new ProblemIRValidationException()
    if(input != x.input) throw new ProblemIRValidationException()
    if(domain != x.codomain) throw new ProblemIRValidationException()
    x
  }
  
  def T: Almap = this
}

//Sum of two linear functionals
case class AlfSum(val arg1: Almap, val arg2: Almap) extends Almap {
  if (arg1.arity != arg2.arity) throw new ProblemIRValidationException()
  if (arg1.input != arg2.input) throw new ProblemIRValidationException()
  if (arg1.domain != arg2.domain) throw new ProblemIRValidationException()
  if (!(arg1.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()
  if (!(arg2.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg1.arity
  val input: Shape = arg1.input
  val domain: Shape = arg1.domain
  val codomain: Shape = ShapeScalar(arity)
  
  def arityOp(op: ArityOp): Almap = AlfSum(arg1.arityOp(op), arg2.arityOp(op))

  def mmpy(x: Almap): Almap = AlfSum(arg1.mmpy(x), arg2.mmpy(x))
  
  def T: Almap = AlmapUtil.sum(arg1.T, arg2.T)
}

//Negation of a linear functional
case class AlfNeg(val arg: Almap) extends Almap {
  if (!(arg.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = ShapeScalar(arity)
  
  def arityOp(op: ArityOp): Almap = AlfNeg(arg.arityOp(op))
  
  def mmpy(x: Almap): Almap = AlfNeg(arg.mmpy(x))

  def T: Almap = AlmapUtil.neg(arg.T)
}

//Scale of a linear functional by a parameter that is itself a linear functional from the input space
case class AlfScale(val arg: Almap, val scale: Almap) extends Almap {
  if (arg.arity != scale.arity) throw new ProblemIRValidationException()
  if (arg.input != scale.input) throw new ProblemIRValidationException()
  if (arg.input != scale.domain) throw new ProblemIRValidationException()
  if (!(arg.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()
  if (!(scale.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = ShapeScalar(arity)
  
  def arityOp(op: ArityOp): Almap = AlfScale(arg.arityOp(op), scale.arityOp(op))
  
  def mmpy(x: Almap): Almap = AlfScale(arg.mmpy(x), scale)
  
  def T: Almap = AlmapUtil.scale(arg.T, scale)
}

//Scale of a linear functional by a constant
case class AlfScaleConstant(val arg: Almap, val scale: Float) extends Almap {
  if (!(arg.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = ShapeScalar(arity)
  
  def arityOp(op: ArityOp): Almap = AlfScaleConstant(arg.arityOp(op), scale)
  
  def mmpy(x: Almap): Almap = AlfScaleConstant(arg.mmpy(x), scale)
  
  def T: Almap = AlmapUtil.scaleconstant(arg.T, scale)
}

//A compound linear map representing a for loop
//One can also think of this as the vertical concatenation of several matrices
case class AlmapFor(val size: Size, val body: Almap) extends Almap {
  if (body.arity != (size.arity + 1)) throw new ProblemIRValidationException()
  val arity: Int = size.arity
  val input: Shape = body.input.demote
  val domain: Shape = body.domain.demote
  val codomain: Shape = ShapeFor(size, body.codomain)
  
  def arityOp(op: ArityOp): Almap = AlmapFor(size.arityOp(op), body.arityOp(op))
  
  def mmpy(x: Almap): Almap = AlmapFor(size, body.mmpy(x.promote))
  
  def T: Almap = AlmapUtil.reduce(size, body.T.mmpy( 
    AlmapIndex(Size.param(arity, arity + 1), AlmapIdentity(input, domain.promote))))
}

//Represents a linear map that indexes the output of another map
case class AlmapIndex(val at: Size, val arg: Almap) extends Almap {
  if (at.arity != arg.arity) throw new ProblemIRValidationException()
  if (!arg.codomain.isInstanceOf[ShapeFor]) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = arg.codomain.asInstanceOf[ShapeFor].body
  
  def arityOp(op: ArityOp): Almap = AlmapIndex(at.arityOp(op), arg.arityOp(op))
  
  def mmpy(x: Almap): Almap = AlmapIndex(at, arg.mmpy(x))
  
  def T: Almap = throw new ProblemIRValidationException()
}

//Represents a sum over a vector-indexed array of linear functionals
case class AlfReduce(val size: Size, val body: Almap) extends Almap {
  if (body.arity != (size.arity + 1)) throw new ProblemIRValidationException()
  if (!body.codomain.isInstanceOf[ShapeScalar]) throw new ProblemIRValidationException()

  val arity: Int = size.arity
  val input: Shape = body.input.demote
  val domain: Shape = body.domain.demote
  val codomain: Shape = ShapeScalar(arity)
  
  def arityOp(op: ArityOp): Almap = AlfReduce(size.arityOp(op), body.arityOp(op))
  
  def mmpy(x: Almap): Almap = AlfReduce(size, body.mmpy(x.promote))
  
  def T: Almap = throw new ProblemIRValidationException()
}

//A compound linear map of different subexpressions
case class AlmapStruct(val body: Seq[Almap]) extends Almap {
  val arity: Int = body(0).arity
  val input: Shape = body(0).input
  val domain: Shape = body(0).domain
  for (b <- body) {
    if (b.arity != arity) throw new ProblemIRValidationException()
    if (b.input != input) throw new ProblemIRValidationException()
    if (b.domain != domain) throw new ProblemIRValidationException()
  }
  val codomain: Shape = ShapeStruct(body map ((x) => x.codomain))
  
  def arityOp(op: ArityOp): Almap = AlmapStruct(body map ((x) => x.arityOp(op)))
  
  def mmpy(x: Almap): Almap = AlmapStruct(body map ((b) => b.mmpy(x)))
  
  def T: Almap = throw new ProblemIRValidationException()
}

//Represents a linear map that accesses some part of the ouput of another map
case class AlmapAccess(val at: Int, val arg: Almap) extends Almap {
  if (!arg.codomain.isInstanceOf[ShapeStruct]) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = arg.codomain.asInstanceOf[ShapeStruct].body(at)
  
  def arityOp(op: ArityOp): Almap = AlmapAccess(at, arg.arityOp(op))
  
  def mmpy(x: Almap): Almap = AlmapAccess(at, arg.mmpy(x))
  
  def T: Almap = throw new ProblemIRValidationException()
}
