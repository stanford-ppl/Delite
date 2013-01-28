package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPOpsGlobal {

  var globalArity: Int = -1
  var globalInputSize: InputDesc = null
  var globalVarSize: IRPoly = null
  var globalArgSize: Seq[IRPoly] = null
  var globalSignumArity: Int = -1

  def globalArityPromote() {
    if ((globalInputSize != null)||(globalVarSize != null)||(globalArgSize != null)) {
      if (globalInputSize.arity != globalArity) throw new IRValidationException()
      if (globalVarSize.arity != globalArity) throw new IRValidationException()
      for(a <- globalArgSize) {
        if(a.arity != globalArity) throw new IRValidationException()
      }
      globalInputSize = globalInputSize.promote
      globalVarSize = globalVarSize.promote
      globalArgSize = globalArgSize map (x => x.promote)
    }
    globalArity += 1
  }

  def globalArityDemote() {
    if ((globalInputSize != null)||(globalVarSize != null)||(globalArgSize != null)) {
      if (globalInputSize.arity != globalArity) throw new IRValidationException()
      if (globalVarSize.arity != globalArity) throw new IRValidationException()
      for(a <- globalArgSize) {
        if(a.arity != globalArity) throw new IRValidationException()
      }
      globalInputSize = globalInputSize.demote
      globalVarSize = globalVarSize.demote
      globalArgSize = globalArgSize map (x => x.demote)
    }
    globalArity -= 1
  }

  def scalar: IRPoly = IRPoly.const(1, globalArity)
  def vector(size: IRPoly): IRPoly = {
    if(size.arity != globalArity) throw new IRValidationException()
    size
  }

  /*
  class Symbol[T >: Null <: HasArity[T], R >: Null] {
    protected[dcp] var binding: T = null
    protected[dcp] def bind(e: T) {
      if (binding != null) throw new IRValidationException()
      if (resolution != null) throw new IRValidationException()
      binding = e
    }
    protected[dcp] var resolution: R = null
    protected[dcp] def rset(r: R) {
      if (resolution != null) throw new IRValidationException()
      if (binding == null) throw new IRValidationException()
      binding = null
      resolution = r
    }
    def resolve: R = {
      if (resolution == null) throw new IRValidationException()
      resolution
    }
  }
  
  implicit def symbol2Timpl[T >: Null <: HasArity[T], R >: Null](s: Symbol[T, R]): T = {
    if (s.binding == null) throw new IRValidationException()
    var lsx: T = s.binding
    while(lsx.arity < globalArity) {
      lsx = lsx.promote
    }
    if (s.binding.arity > globalArity) throw new IRValidationException()
    lsx
  }
  */

}