package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPOpsGlobal {

  var globalArity: Int = -1
  var globalInputShape: Shape = null
  var globalVarShape: Shape = null

  def globalArityPromote() {
    if ((globalInputShape != null)||(globalVarShape != null)) {
      if (globalInputShape.arity != globalArity) throw new IRValidationException()
      if (globalVarShape.arity != globalArity) throw new IRValidationException()
      globalInputShape = globalInputShape.promote
      globalVarShape = globalVarShape.promote
    }
    globalArity += 1
  }

  def globalArityDemote() {
    if ((globalInputShape != null)||(globalVarShape != null)) {
      if (globalInputShape.arity != globalArity) throw new IRValidationException()
      if (globalVarShape.arity != globalArity) throw new IRValidationException()
      globalInputShape = globalInputShape.demote
      globalVarShape = globalVarShape.demote
    }
    globalArity -= 1
  }
  
}