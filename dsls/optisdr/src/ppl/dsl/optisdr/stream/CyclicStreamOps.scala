package ppl.dsl.optisdr.stream

import java.io._

import scala.reflect.Manifest
import scala.reflect.SourceContext

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

import ppl.delite.framework.ops.DeliteCollectionOpsExp
import ppl.delite.framework.datastruct.scala.DeliteCollection

import ppl.dsl.optisdr._
import ppl.dsl.optila.DenseVector

trait CyclicStreamOps extends Variables {
  this: OptiSDR =>
  
  def cyclicstream_new[A:Manifest](x: Rep[Array[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[CyclicStream[A]]
  def cyclicstream_new[A:Manifest](x: Rep[DenseVector[A]], n: Rep[Int])(implicit ctx: SourceContext, o: Overloaded1): Rep[CyclicStream[A]]
  
  def cyclicstream_apply[A:Manifest](x: Rep[Stream[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  
  def cyclicstream_data[A:Manifest](x: Rep[CyclicStream[A]])(implicit ctx: SourceContext): Rep[Array[A]]
  def cyclicstream_offset[A:Manifest](x: Rep[CyclicStream[A]])(implicit ctx: SourceContext): Rep[Int]
}

trait CyclicStreamOpsExp extends CyclicStreamOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: OptiSDRExp =>
  
  case class CyclicStreamNew[A:Manifest](x: Exp[Array[A]], n: Exp[Int]) extends DefWithManifest[A,CyclicStream[A]]
  case class CyclicStreamNewWithVector[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int]) extends DefWithManifest[A,CyclicStream[A]]
  
  def cyclicstream_new[A:Manifest](x: Exp[Array[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(CyclicStreamNew(x, n))
  def cyclicstream_new[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int])(implicit ctx: SourceContext, o: Overloaded1) = reflectPure(CyclicStreamNewWithVector(x, n))
  
  case class CyclicStreamApply[A:Manifest](x: Exp[CyclicStream[A]], n: Exp[Int]) extends DefWithManifest[A,A]
  
  case class CyclicStreamData[A:Manifest](x: Exp[CyclicStream[A]]) extends DefWithManifest[A,Array[A]]
  case class CyclicStreamOffset[A:Manifest](x: Exp[CyclicStream[A]]) extends DefWithManifest[A,Int]
  
  def cyclicstream_apply[A:Manifest](x: Rep[Stream[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  
  def cyclicstream_data[A:Manifest](x: Rep[CyclicStream[A]])(implicit ctx: SourceContext) = reflectPure(CyclicStreamData(x))
  def cyclicstream_offset[A:Manifest](x: Rep[CyclicStream[A]])(implicit ctx: SourceContext) = reflectPure(CyclicStreamOffset(x))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@CyclicStreamApply(x,n) => reflectPure(CyclicStreamApply(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@CyclicStreamData(x) => reflectPure(CyclicStreamData(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@CyclicStreamOffset(x) => reflectPure(CyclicStreamOffset(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    
    case Reflect(e@CyclicStreamNew(x,n), u, es) => reflectMirrored(Reflect(CyclicStreamNew(f(x),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@CyclicStreamNewWithVector(x,n), u, es) => reflectMirrored(Reflect(CyclicStreamNewWithVector(f(x),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    
    case Reflect(e@CyclicStreamApply(x,n), u, es) => reflectMirrored(Reflect(CyclicStreamApply(f(x),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@CyclicStreamData(x), u, es) => reflectMirrored(Reflect(CyclicStreamData(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@CyclicStreamOffset(x), u, es) => reflectMirrored(Reflect(CyclicStreamOffset(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait CyclicStreamOpsExpOpt extends CyclicStreamOpsExp {
  this: OptiSDRExp =>
}

trait BaseGenCyclicStreamOps extends GenericFatCodegen {
  val IR: CyclicStreamOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // WHAT IS THIS?
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenCyclicStreamOps extends BaseGenCyclicStreamOps with ScalaGenFat {
  val IR: CyclicStreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case c@CyclicStreamNew(x,n) => emitValDef(sym, remap("generated.scala.CyclicStream[" + remap(c.mA) + "]")+"(" + quote(x) + "," + quote(n) + ")")
    case c@CyclicStreamNewWithVector(x,n) => emitValDef(sym, remap("generated.scala.CyclicStream[" + remap(c.mA) + "]")+"(" + quote(x) + "._data," + quote(n) + ")")
    
    case CyclicStreamApply(x, n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case CyclicStreamData(x) => emitValDef(sym, quote(x) + ".data")
    case CyclicStreamOffset(x) => emitValDef(sym, quote(x) + ".offset")
    
    case _ => super.emitNode(sym, rhs)
  }
}