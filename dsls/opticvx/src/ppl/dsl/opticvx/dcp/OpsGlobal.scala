package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPOpsGlobal {

  var globalArity: Int = -1
  var globalInputSize: IRPoly = null
  var globalVarSize: IRPoly = null

  def globalArityPromote() {
    if ((globalInputSize != null)||(globalVarSize != null)) {
      if (globalInputSize.arity != globalArity) throw new IRValidationException()
      if (globalVarSize.arity != globalArity) throw new IRValidationException()
      globalInputSize = globalInputSize.promote
      globalVarSize = globalVarSize.promote
    }
    globalArity += 1
  }

  def globalArityDemote() {
    if ((globalInputSize != null)||(globalVarSize != null)) {
      if (globalInputSize.arity != globalArity) throw new IRValidationException()
      if (globalVarSize.arity != globalArity) throw new IRValidationException()
      globalInputSize = globalInputSize.demote
      globalVarSize = globalVarSize.demote
    }
    globalArity -= 1
  }


  class Symbol[T >: Null <: HasArity[T]] {
    var binding: T = null
    def bind(e: T) {
      if (binding != null) throw new IRValidationException()
      binding = e
    }
  }
  
  implicit def symbol2Timpl[T >: Null <: HasArity[T]](s: Symbol[T]): T = {
    if (s.binding == null) throw new IRValidationException()
    var lsx: T = s.binding
    while(lsx.arity < globalArity) {
      lsx = lsx.promote
    }
    if (s.binding.arity > globalArity) throw new IRValidationException()
    lsx
  }

}