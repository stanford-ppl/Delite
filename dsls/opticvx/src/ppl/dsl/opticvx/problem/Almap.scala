// Almap = Abstract Linear MAP
// This represents a linear map as a composition of a set of linear mapping primitives

package ppl.dsl.opticvx.problem

import scala.collection.immutable.Seq

trait Almap {
  //The number of integer parameters in this expression's scope
  val arity: Int
  //The shape of the input parameters used by this map
  val input: Shape
  //The domain and codomain of this map
  val domain: Shape
  val codomain: Shape

  //Constraints that all the shape properties must share the map's arity
  if (input.arity != arity) throw new ProblemIRValidationException()
  if (domain.arity != arity) throw new ProblemIRValidationException()
  if (codomain.arity != arity) throw new ProblemIRValidationException()

  //Promote this map by increasing its arity without affecting its other semantics
  //def promote: Almap

  //The transpose
  //def T: Almap

  //Sum of two same-shaped maps
  /*
  def +(a: Almap): Almap = {
    if (a.arity != arity) throw new ProblemIRValidationException()
    if (a.input != input) throw new ProblemIRValidationException()
    if (a.domain != domain) throw new ProblemIRValidationException()
    if (a.codomain != codomain) throw new ProblemIRValidationException()
    codomain match {
      case ShapeScalar(ar) => AlfSum(this, a)
      case ShapeFor(sz, b) => AlmapFor(sz, AlmapIndex(Size.param(arity, arity + 1), this.promote) + AlmapIndex(Size.param(arity, arity + 1), a.promote))
    }
  }
  */

  //Negation of a map
  //def unary_-(): Almap
}


//The identity map over a space
case class AlmapIdentity(val domain: Shape, val input: Shape) extends Almap {
  val arity: Int = domain.arity
  val codomain: Shape = domain

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

  //def T: Almap = arg1.T + arg2.T
}

//Negation of a linear functional
case class AlfNeg(val arg: Almap) extends Almap {
  if (!(arg.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = ShapeScalar(arity)

  //def T: Almap = -arg.T
}

//Scale of a linear functional by a parameter that is itself a linear functional from the input space
case class AlfScale(val arg: Almap, val scale: Almap) {
  if (arg.arity != scale.arity) throw new ProblemIRValidationException()
  if (arg.input != scale.input) throw new ProblemIRValidationException()
  if (arg.input != scale.domain) throw new ProblemIRValidationException()
  if (!(arg.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()
  if (!(scale.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = ShapeScalar(arity)
}

//Scale of a linear functional by a constant
case class AlfScaleConstant(val arg: Almap, val scale: Float) {
  if (!(arg.codomain.isInstanceOf[ShapeScalar])) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = ShapeScalar(arity)
}

//A compound linear map representing a for loop
case class AlmapFor(val size: Size, val body: Almap) extends Almap {
  if (body.arity != (size.arity + 1)) throw new ProblemIRValidationException()
  if (!body.input.demotable) throw new ProblemIRValidationException()
  if (!body.domain.demotable) throw new ProblemIRValidationException()

  val arity: Int = size.arity
  val input: Shape = body.input.demote
  val domain: Shape = body.domain.demote
  val codomain: Shape = ShapeFor(size, body.codomain)
}

//Represents a linear map that indexes the output of another map
case class AlmapIndex(val at: Size, val arg: Almap) extends Almap {
  if (at.arity != arg.arity) throw new ProblemIRValidationException()
  if (!arg.codomain.isInstanceOf[ShapeFor]) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = arg.codomain.asInstanceOf[ShapeFor].body
}

//Represents a sum over a vector-indexed array of linear functionals
case class AlfReduce(val size: Size, val body: Almap) extends Almap {
  if (body.arity != (size.arity + 1)) throw new ProblemIRValidationException()
  if (!body.codomain.isInstanceOf[ShapeScalar]) throw new ProblemIRValidationException()
  if (!body.input.demotable) throw new ProblemIRValidationException()
  if (!body.domain.demotable) throw new ProblemIRValidationException()

  val arity: Int = size.arity
  val input: Shape = body.input.demote
  val domain: Shape = body.domain.demote
  val codomain: Shape = ShapeScalar(arity)
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
}

//Represents a linear map that accesses some part of the ouput of another map
case class AlmapAccess(val at: Int, val arg: Almap) extends Almap {
  if (!arg.codomain.isInstanceOf[ShapeStruct]) throw new ProblemIRValidationException()

  val arity: Int = arg.arity
  val input: Shape = arg.input
  val domain: Shape = arg.domain
  val codomain: Shape = arg.codomain.asInstanceOf[ShapeStruct].body(at)
}
