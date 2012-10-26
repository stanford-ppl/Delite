package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Seq

trait DCPShape {

  sealed trait ArityOp
  case class ArityOpRemoveParam(idx: Int) extends ArityOp
  case class ArityOpAddParam(idx: Int) extends ArityOp

  trait HasArity[T] {
    val arity: Int
    def demote: T = removeParam(arity - 1)
    def promote: T = addParam(arity)
    def removeParam(idx: Int): T = arityOp(ArityOpRemoveParam(idx))
    def addParam(idx: Int): T = arityOp(ArityOpAddParam(idx))
    def arityOp(op: ArityOp): T
  }

  object Size {
    def param(idx: Int, arity: Int): Size = Size(0, Seq[Int]().padTo(arity, 0).updated(idx, 1))
    def const(c: Int, arity: Int): Size = Size(0, Seq[Int]().padTo(arity, 0))
  }

  case class Size(val const: Int, val coeffs: Seq[Int]) extends HasArity[Size] {
    val arity: Int = coeffs.length
    def arityOp(op: ArityOp): Size = op match {
      case ArityOpRemoveParam(idx) => {
        if (coeffs(idx) != 0) throw new DCPIRValidationException()
        Size(const, coeffs.take(idx) ++ coeffs.drop(idx+1))
      }
      case ArityOpAddParam(idx) => Size(const, (coeffs.take(idx) :+ 0) ++ coeffs.drop(idx))
    }

    if (const < 0) throw new DCPIRValidationException()
    for (c <- coeffs) {
      if (c < 0) throw new DCPIRValidationException()
    }
  }

  sealed trait ShapeWith[T] extends HasArity[ShapeWith[T]] {
    def morph[U](fx: (T) => U): ShapeWith[U]
  }

  case class ShapeScalarWith[T](val arity: Int, val desc: T) extends ShapeWith[T] {
    def arityOp(op: ArityOp): ShapeWith[T] = op match {
      case ArityOpRemoveParam(idx) => {
        if ((arity == 0)||(idx >= arity)) throw new DCPIRValidationException()
        ShapeScalarWith[T](arity - 1, desc)
      }
      case ArityOpAddParam(idx) => ShapeScalarWith[T](arity + 1, desc)
    }
    def morph[U](fx: (T) => U): ShapeWith[U] = ShapeScalarWith[U](arity, fx(desc))
  }
  case class ShapeForWith[T](val size: Size, val body: ShapeWith[T]) extends ShapeWith[T] {
    val arity = size.arity
    if (body.arity != (arity + 1)) throw new DCPIRValidationException()
    def arityOp(op: ArityOp): ShapeWith[T] = ShapeForWith[T](size.arityOp(op), body.arityOp(op))
  }
  case class ShapeStructWith[T](val body: Seq[ShapeWith[T]]) extends ShapeWith[T] {
    val arity = body(0).arity
    for (b <- body) {
      if (b.arity != arity) throw new DCPIRValidationException()
    }
    def arityOp(op: ArityOp): ShapeWith[T] = ShapeStructWith[T](body map ((x) => x.arityOp(op)))
  }

}

/*
case class Size(val const: Int, val coeffs: Seq[Int]) {
  def nIntParams: Int = coeffs.length

  if (const < 0) throw new DCPIRValidationException()
  for (c <- coeffs) {
    if (c < 0) throw new ProblemIRValidationException()
  }
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
*/
