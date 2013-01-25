
package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class InputDesc(val dims: Seq[IRPoly], val domain: IRPoly, val codomain: IRPoly) extends HasArity[InputDesc] {
  val arity: Int = domain.arity - dims.length
  if(domain.arity != codomain.arity) throw new IRValidationException()
  for(i <- 0 until dims.length) {
    if(dims(i).arity != arity - (dims.length - i)) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): InputDesc = InputDesc(
    for(i <- 0 until dims.length) yield dims(i).arityOp(op.promoteBy(i)),
    domain.arityOp(op.promoteBy(dims.length)),
    codomain.arityOp(op.promoteBy(dims.length)))
}

case class InputOp(val input: Seq[InputDesc], val xs: Seq[Almap]) extends HasInput[InputOp] {
  for(x <- xs) {
    if(x.input != input) throw new IRValidationException()
  }

  def inputOp(op: InputOp): Input = InputOp(op.input, xs map (x => x.inputOp(op)))
}

trait HasInput[T] extends HasArity[T] {
  val input: Seq[InputDesc]

  def inputOp(op: InputOp): T
}