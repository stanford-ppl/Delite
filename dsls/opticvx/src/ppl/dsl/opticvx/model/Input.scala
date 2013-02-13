
package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class InputArgDesc(val dims: Seq[IRPoly], val domain: IRPoly, val codomain: IRPoly) extends HasArity[InputArgDesc] {
  val arity: Int = domain.arity - dims.length
  if(domain.arity != codomain.arity) throw new IRValidationException()
  for(i <- 0 until dims.length) {
    if(dims(i).arity != arity - (dims.length - i)) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): InputArgDesc = InputArgDesc(
    for(i <- 0 until dims.length) yield dims(i).arityOp(op.promoteBy(i)),
    domain.arityOp(op.promoteBy(dims.length)),
    codomain.arityOp(op.promoteBy(dims.length)))
}

case class MemoryArgDesc(val dims: Seq[IRPoly], val size: IRPoly) extends HasArity[MemoryArgDesc] {
  val arity: Int = size.arity - dims.length
  for(i <- 0 until dims.length) {
    if(dims(i).arity != arity - (dims.length - i)) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): MemoryArgDesc = MemoryArgDesc(
    for(i <- 0 until dims.length) yield dims(i).arityOp(op.promoteBy(i)),
    size.arityOp(op.promoteBy(dims.length)))
}

case class InputDesc(val arity: Int, val args: Seq[InputArgDesc], val memory: Seq[MemoryArgDesc]) extends HasArity[InputDesc] {
  for(a <- args) {
    if(a.arity != arity) throw new IRValidationException()
  }
  for(m <- memory) {
    if(m.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): InputDesc = InputDesc(op.arity, args map (a => a.arityOp(op)), memory map (a => a.arityOp(op)))
}

case class InputOp(val input: InputDesc, val xs: Seq[Almap], val ms: Seq[AVector]) extends HasInput[InputOp] {
  val arity = input.arity
  for(x <- xs) {
    if(x.input != input) throw new IRValidationException()
  }
  for(m <- ms) {
    if(m.input != input) throw new IRValidationException()
  }

  def inputOp(op: InputOp): InputOp = InputOp(op.input, xs map (x => x.inputOp(op)), ms map (m => m.inputOp(op)))

  def arityOp(op: ArityOp): InputOp = InputOp(input.arityOp(op), xs map (x => x.arityOp(op)), ms map (m => m.arityOp(op)))
}

trait HasInput[T] extends HasArity[T] {
  val input: InputDesc

  def inputOp(op: InputOp): T

  def isMemoryless: Boolean = (input.memory == Seq())
}