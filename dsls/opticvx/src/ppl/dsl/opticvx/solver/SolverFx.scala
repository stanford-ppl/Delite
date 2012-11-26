package ppl.dsl.opticvx.solver

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait SVector extends HasArity[SVector] {
  val len: IRPoly
}

case class SVectorZero(val len: IRPoly) extends SVector {
  val arity: Int = len.arity  
  def arityOp(op: ArityOp): SVector = SVectorZero(len.arityOp(op))
}

case class SVectorSum(val arg1: SVector, val arg2: SVector) extends SVector {
  val arity: Int = arg1.arity
  val len: IRPoly = arg1.len

  if(arg1.len != arg2.len) throw new IRValidationException()

  def arityOp(op: ArityOp): SVector = SVectorSum(arg1.arityOp(op), arg2.arityOp(op))
}
