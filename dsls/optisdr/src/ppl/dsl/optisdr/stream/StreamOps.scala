package ppl.dsl.optisdr.stream

import java.io._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

import ppl.dsl.optisdr._

trait SDRStreamOps extends Variables {
  this: OptiSDR =>
  
  // Convert from Rep[Stream] to our Stream ops
  implicit def repToSDRStreamOps[A:Manifest](x: Rep[Stream[A]]) = new SDRStreamOpsCls(x)
  implicit def varToSDRStreamOps[A:Manifest](x: Var[Stream[A]]) = new SDRStreamOpsCls(readVar(x))
  
  // Objects methods
  class SDRStreamOpsCls[A:Manifest](val x: Rep[Stream[A]]) { 
    def <<(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = stream_lshift(x, y)
    def >>(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = stream_rshift(x, y)
  }
  
  def stream_lshift[A:Manifest:BitArith](x: Rep[Stream[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[Stream[A]]
  def stream_rshift[A:Manifest:BitArith](x: Rep[Stream[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[Stream[A]]
  
  object FakeStreamVector {
    def apply[A:Manifest](xs: Rep[A]*) = fsv_obj_new(xs : _*)
  }
  
  def fsv_obj_new[A:Manifest](xs: Rep[A]*): Rep[Stream[A]]
}

trait SDRStreamOpsExp extends SDRStreamOps with VariablesExp with BaseFatExp {
  this: OptiSDRExp =>
  
  case class SDRStreamLShift[A:Manifest:BitArith](x: Exp[Stream[A]], y: Exp[Int]) extends Def[Stream[A]]
  case class SDRStreamRShift[A:Manifest:BitArith](x: Exp[Stream[A]], y: Exp[Int]) extends Def[Stream[A]]
  
  def stream_lshift[A:Manifest:BitArith](x: Exp[Stream[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRStreamLShift(x,y))
  def stream_rshift[A:Manifest:BitArith](x: Exp[Stream[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRStreamRShift(x,y))
  
  case class FSVObjNew[A:Manifest](xs: Exp[A]*) extends Def[Stream[A]] {
    def a = manifest[A]
  }
}

trait SDRStreamOpsExpOpt extends SDRStreamOpsExp {
  this: OptiSDRExp =>
}

trait BaseGenStreamOps extends GenericFatCodegen {
  val IR: SDRStreamOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // WHAT IS THIS?
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenStreamOps extends BaseGenStreamOps with ScalaGenFat {
  val IR: SDRStreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case v@FSVObjNew(xs @ _*) => emitValDef(sym, "new " + remap("generated.scala.FakeStreamVector[" + remap(v.a) + "]")+"(" + xs.map(quote).reduceLeft(_+","+_) + ")")        
    
    case _ => super.emitNode(sym, rhs)
  }
}