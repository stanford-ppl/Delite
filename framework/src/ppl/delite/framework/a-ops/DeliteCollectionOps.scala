package ppl.delite.framework.ops

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.Interfaces

trait DeliteCollectionOps extends Interfaces with Variables {
    
  // AKS: Is DCInterfaceOps still useful for anything? Are they meant to guarantee that dc_* static call will succeed?
  // should this be split into read and write interfaces?
  trait DCInterfaceOps[+T,A] extends InterfaceOps[T] {
    def dcSize(implicit ctx: SourceContext): Rep[Int] 
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] 
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  }

  trait DCInterface[+T,A] extends Interface[T] {
    val ops: DCInterfaceOps[T,A]
  }
  
  implicit def interfaceToDCOps[A:Manifest](intf: Interface[DeliteCollection[A]]) = new DeliteCollectionInterfaceOps(intf.asInstanceOf[DCInterface[DeliteCollection[A],A]])
  
  // unlike before, don't assume that we know how to generate dcSize,dcApply,dcUpdate at run-time
  class DeliteCollectionInterfaceOps[A:Manifest](val intf: DCInterface[DeliteCollection[A],A]) {
    def dcSize(implicit ctx: SourceContext) = intf.ops.dcSize
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext) = intf.ops.dcApply(n) 
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = intf.ops.dcUpdate(n,y)
  }
  
  def dc_size[A:Manifest](x: Rep[DeliteCollection[A]])(implicit ctx: SourceContext): Rep[Int]
  def dc_apply[A:Manifest](x: Rep[DeliteCollection[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def dc_update[A:Manifest](x: Rep[DeliteCollection[A]], n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
}

trait DeliteCollectionOpsExp extends DeliteCollectionOps with VariablesExp with ExceptionOpsExp with BaseFatExp with EffectExp { this: DeliteOpsExp =>
  case class DeliteCollectionSize[A:Manifest](x: Exp[DeliteCollection[A]]) extends Def[Int]
  case class DeliteCollectionApply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) extends Def[A] {
    def mA = manifest[A]
  }
  case class DeliteCollectionUpdate[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class DeliteCollectionAppend[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class DeliteCollectionUnsafeSetData[A:Manifest](x: Exp[DeliteCollection[A]], d: Exp[Array[A]]) extends Def[Unit] { // legacy...
    def m = manifest[A]
  }

  /**
   * Default delite collection op implementations
   * TODO: provide implementations for DeliteArray
   */
    
  // dc_size is currently unused. do we need it? if we keep it, should it be renamed to dc_physical_size?
  def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x match { // TODO: move to Opt trait ?
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    //case Def(Reflect(e: DeliteOpMap[_,_,_], _,_)) => e.size // reasonable?
    //case Def(Reflect(e: DeliteOpZipWith[_,_,_,_], _,_)) => e.size // reasonable?
    case _ =>
      /*throw new RuntimeException*/printlog("warning: no static implementation found for dc_size on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get)
      reflectPure(DeliteCollectionSize(x))
  }
  def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    /*throw new RuntimeException*/printlog("warning: no static implementation found for dc_apply on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
    reflectPure(DeliteCollectionApply(x,n))
  }
  def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    /*throw new RuntimeException*/printlog("warning: no static implementation found for dc_update on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get)
    reflectWrite(x)(DeliteCollectionUpdate(x,n,y))
  }
  def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (hasConditions) ParBuffer else ParFlat // default
  }
  
  // -- ParBuffer methods
   
  def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    //reflectPure(DeliteCollectionSetLogicalSize(x,y))    
    fatal(unit("dc_set_logical_size called without any implementation on " + x))    
  }  
  /* returns true if the element was accepted and actually appended, false otherwise */
  def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Boolean] = {
    // reflectWrite(x)(DeliteCollectionAppend(x,i,y))
    // unit(true)
    fatal(unit("dc_append called without any implementation on " + x))    
  }  
  def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    fatal(unit("dc_alloc called without any implementation on " + x))
  }  
  def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    fatal(unit("dc_copy called without any implementation on " + src))
  }    

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteCollectionApply(x, n) => dc_apply(f(x), f(n))(e.mA,ctx)
    case DeliteCollectionSize(x) => dc_size(f(x))
    case Reflect(e@DeliteCollectionApply(l,r), u, es) => reflectMirrored(Reflect(DeliteCollectionApply(f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteCollectionSize(l), u, es) => reflectMirrored(Reflect(DeliteCollectionSize(f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteCollectionUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DeliteCollectionUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteCollectionAppend(l,i,r), u, es) => reflectMirrored(Reflect(DeliteCollectionAppend(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(dc@DeliteCollectionUnsafeSetData(x, d), u, es) => reflectMirrored(Reflect(DeliteCollectionUnsafeSetData(f(x),f(d))(dc.m), mapOver(f,u), f(es)))(mtype(manifest[Unit]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
  
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => Nil
    case DeliteCollectionUpdate(a,i,x) => Nil
    case DeliteCollectionAppend(a,i,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => Nil
    case DeliteCollectionUpdate(a,i,x) => syms(x)
    case DeliteCollectionAppend(a,i,x) => syms(x)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => syms(a)
    case DeliteCollectionUpdate(a,i,x) => Nil
    case DeliteCollectionAppend(a,i,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => Nil
    case DeliteCollectionUpdate(a,i,x) => syms(a)
    case DeliteCollectionAppend(a,i,x) => syms(a)
    case _ => super.copySyms(e)
  }
}

trait BaseGenDeliteCollectionOps extends GenericFatCodegen {
  val IR: DeliteCollectionOpsExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
    case DeliteCollectionApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
}
trait ScalaGenDeliteCollectionOps extends BaseGenDeliteCollectionOps with ScalaGenFat {
  val IR: DeliteCollectionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case DeliteCollectionSize(x) => emitValDef(sym, quote(x) + ".dcSize")
      case DeliteCollectionApply(x,n) => emitValDef(sym, quote(x) + ".dcApply(" + quote(n) + ")")
      case DeliteCollectionUpdate(x,n,y) => emitValDef(sym, quote(x) + ".dcUpdate(" + quote(n) + "," + quote(y) + ")")      
      case DeliteCollectionAppend(x,n,y) => emitValDef(sym, "{ " + quote(x) + " += (" + quote(y) + "); true }")      
      case DeliteCollectionUnsafeSetData(x,d) => emitValDef(sym, quote(x) + ".unsafeSetData(" + quote(d) + "," + quote(d) + ".length)")
      case _ => super.emitNode(sym, rhs)
    }

  }
}

trait CudaGenDeliteCollectionOps extends BaseGenDeliteCollectionOps with CudaGenEffect {
  val IR: DeliteCollectionOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case DeliteCollectionSize(x) => emitValDef(sym, quote(x) + ".dcSize()")
      case DeliteCollectionApply(x,n) => emitValDef(sym, quote(x) + ".dcApply(" + quote(n) + ")")
      case DeliteCollectionUpdate(x,n,y) => stream.println(quote(x) + ".dcUpdate(" + quote(n) + "," + quote(y) + ");")
      case DeliteCollectionUnsafeSetData(x,d) =>
        if(processingHelperFunc) stream.println(quote(x) + "_ptr->unsafeSetData(" + quote(d) + "," + quote(d) + "->length);")
        else throw new GenerationFailedException("CudaGen: UnsafeSetData can be done only within helper functions.")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait OpenCLGenDeliteCollectionOps extends BaseGenDeliteCollectionOps with OpenCLGenEffect {
  val IR: DeliteCollectionOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case DeliteCollectionSize(x) => emitValDef(sym, "%s_size(%s)".format(remap(x.tp),quote(x)))
      case DeliteCollectionApply(x,n) => emitValDef(sym, "%s_dcApply(%s,%s)".format(remap(x.tp), quote(x), quote(n)))
      //case DeliteCollectionUpdate(x,n,y) => emitValDef(sym, "%s_dcUpdate(%s,%s,%s)".format(remap(x.tp),quote(x),quote(n),quote(y)))
      case DeliteCollectionUpdate(x,n,y) => stream.println("%s_dcUpdate(%s,%s,%s);".format(remap(x.tp),quote(x),quote(n),quote(y)))
      case DeliteCollectionUnsafeSetData(x,d) => emitValDef(sym, quote(x) + ".unsafeSetData(" + quote(d) + "," + quote(d) + " ->length);")
      case _ => super.emitNode(sym, rhs)
    }
  }
}



