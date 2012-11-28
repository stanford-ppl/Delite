package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPOpsDefinite extends DCPOps {
  
  type ParamDesc = Int
  type InputDesc = InputDescDefinite

  case class InputDescDefinite(val size: IRPoly, val data: (Int)=>Double) extends HasSize

  implicit def double2inputdesc(x: Double) = InputDescDefinite(IRPoly.const(1, globalArity),
    (i: Int) => {
      if(i != 0) throw new IRValidationException()
      x
      })

}