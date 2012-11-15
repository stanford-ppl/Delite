package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


trait Cone extends HasArity[Cone] {
  val size: IRPoly
}

//The trivial scalar cone (the only proper cone over R)
case class ConeNonNegative(val arity: Int) extends Cone {
  val size: IRPoly = IRPoly.const(1,arity)
  
  def arityOp(op: ArityOp): Cone = ConeNonNegative(op(size).arity)
}

//Second order cone
case class ConeSecondOrder(val dim: IRPoly) extends Cone {
  val arity: Int = dim.arity
  val size: IRPoly = dim + IRPoly.const(1, arity)
    
  def arityOp(op: ArityOp): Cone = ConeSecondOrder(dim.arityOp(op))
  
  if (size.arity != arity) throw new IRValidationException()
}

//Cartesian-product of cones
case class ConeProduct(val arg1: Cone, val arg2: Cone) extends Cone {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size + arg2.size

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Cone = ConeProduct(arg1.arityOp(op), arg2.arityOp(op))
}

//For-loop product of cones
case class ConeFor(val len: IRPoly, val body: Cone) extends Cone {
  val arity: Int = len.arity
  val size: IRPoly = body.size.sum(arity).substituteAt(arity, len)

  if(body.arity != (len.arity + 1)) throw new IRValidationException()
  if (size.arity != arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Cone = ConeFor(len.arityOp(op), body.arityOp(op))
}
