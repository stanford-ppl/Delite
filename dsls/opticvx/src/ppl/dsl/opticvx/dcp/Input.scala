package ppl.dsl.opticvx.dcp

import scala.virtualization.lms.common.ScalaOpsPkg

import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPInput {
  self: DCPShape =>
  
  type RepInt
  type RepDouble
  
  val repint0: RepInt
  
  trait InputDesc extends HasArity[InputDesc] {
    val shape: Shape
  }
  
  case class InputDescScalar(val arity: Int, val input: RepDouble) extends InputDesc {
    val shape: Shape = ShapeScalar(arity)
    def arityOp(op: ArityOp): InputDesc = InputDescScalar(op(arity), input)
  }
  
  case class InputDescFor(val size: Size, val body: (RepInt) => InputDesc) extends InputDesc {
    val arity: Int = size.arity
    val shape: Shape = {
      globalArityPromote()
      val iid: InputDesc = body(repint0)
      globalArityDemote()
      ShapeFor(size, iid.shape)
    }
    def arityOp(op: ArityOp): InputDesc = InputDescFor(size.arityOp(op), body)
  }

  implicit def repdbl2inputdesc(input: RepDouble): InputDesc = InputDescScalar(globalArity, input)
  def ifor(size: Size, body: (RepInt) => InputDesc): InputDesc = InputDescFor(size, body)
}
