package ppl.dsl.optisdr.primitive

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optisdr._

trait SoftBitOps extends Variables {
  this: OptiSDR =>
    
  // Static methods
  object SoftBit {
    def apply(x: Byte)(implicit ctx: SourceContext) = softbit_new(unit(x))
    def apply(x: Rep[Byte])(implicit ctx: SourceContext) = softbit_new(x)
  }
  
  // Constructor
  def softbit_new(x: Rep[Byte])(implicit ctx: SourceContext) : Rep[SoftBit]
}

trait SoftBitOpsExp extends SoftBitOps {
  this: OptiSDRExp =>
  
  // Object creation
  case class SoftBitNew(x: Exp[Byte]) extends Def[SoftBit]
  
  def softbit_new(x: Exp[Byte])(implicit ctx: SourceContext) = reflectPure(SoftBitNew(x))
}
