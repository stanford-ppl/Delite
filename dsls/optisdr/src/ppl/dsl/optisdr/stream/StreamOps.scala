package ppl.dsl.optisdr.stream

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optisdr._

trait SDRStreamOps extends Variables {
  this: OptiSDR =>
  
  // Convert from Rep[Stream] to our Stream ops
  implicit def repToSDRStreamOps[A:Manifest](x: Rep[DenseStream[A]]) = new SDRStreamOpsCls(x)
  implicit def varToSDRStreamOps[A:Manifest](x: Var[DenseStream[A]]) = new SDRStreamOpsCls(readVar(x))
  
  // Objects methods
  class SDRStreamOpsCls[A:Manifest](val x: Rep[DenseStream[A]]) { 
    def <<(y: Rep[Int])(implicit ctx: SourceContext) = stream_lshift(x, y)
    def >>(y: Rep[Int])(implicit ctx: SourceContext) = stream_rshift(x, y)
  }
  
  def stream_lshift[A:Manifest:BitArith](x: Rep[DenseStream[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[Stream[A]]
  def stream_rshift[A:Manifest:BitArith](x: Rep[DenseStream[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[Stream[A]]
}

trait SDRStreamOpsExp extends SDRStreamOps {
  this: OptiSDRExp =>
  
  case class SDRStreamLShift[A:Manifest:BitArith](x: Exp[DenseStream[A]], y: Exp[Int]) extends Def[Stream[A]]
  case class SDRStreamRShift[A:Manifest:BitArith](x: Exp[DenseStream[A]], y: Exp[Int]) extends Def[Stream[A]]
  
  def stream_lshift[A:Manifest:BitArith](x: Exp[DenseStream[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRStreamLShift(x,y))
  def stream_rshift[A:Manifest:BitArith](x: Exp[DenseStream[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRStreamRShift(x,y))
}

trait SDRStreamOpsExpOpt extends SDRStreamOpsExp {
  this: OptiSDRExp =>
}