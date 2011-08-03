package ppl.delite.framework.ops

import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter
import scala.virtualization.lms.common.{EffectExp, BaseFatExp, Base, ScalaGenFat}
import scala.virtualization.lms.internal.{GenericFatCodegen}

trait DeliteCollectionOps extends Base {
  implicit def dcToDcOps[A:Manifest](x: Rep[DeliteCollection[A]]) = new deliteCollectionOpsCls(x)
  
  class deliteCollectionOpsCls[A:Manifest](x: Rep[DeliteCollection[A]]) {
    def size = dc_size(x)
    def apply(n: Rep[Int]) = dc_apply(x,n)
    def update(n: Rep[Int], y: Rep[A]) = dc_update(x,n,y)
  }
  
  def dc_size[A:Manifest](x: Rep[DeliteCollection[A]]): Rep[Int]
  def dc_apply[A:Manifest](x: Rep[DeliteCollection[A]], n: Rep[Int]): Rep[A]
  def dc_update[A:Manifest](x: Rep[DeliteCollection[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
}

trait DeliteCollectionOpsExp extends DeliteCollectionOps with BaseFatExp with EffectExp { this: DeliteOpsExp =>
  case class DeliteCollectionSize[A:Manifest](x: Exp[DeliteCollection[A]]) extends Def[Int]
  case class DeliteCollectionApply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) extends Def[A]
  case class DeliteCollectionUpdate[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]

  def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]) = x match { // TODO: move to Opt trait ?
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    //case Def(Reflect(e: DeliteOpMap[_,_,_], _,_)) => e.size // reasonable?
    //case Def(Reflect(e: DeliteOpZipWith[_,_,_,_], _,_)) => e.size // reasonable?
    case _ => reflectPure(DeliteCollectionSize(x))
  }
  def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = reflectPure(DeliteCollectionApply(x,n))
  def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) = reflectWrite(x)(DeliteCollectionUpdate(x,n,y))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case DeliteCollectionApply(x, n) => dc_apply(f(x), f(n))
    case DeliteCollectionSize(x) => dc_size(f(x))
    case Reflect(DeliteCollectionApply(l,r), u, es) => reflectMirrored(Reflect(DeliteCollectionApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteCollectionSize(l), u, es) => reflectMirrored(Reflect(DeliteCollectionSize(f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
  
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => Nil
    case DeliteCollectionUpdate(a,i,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => Nil
    case DeliteCollectionUpdate(a,i,x) => syms(x)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => syms(a)
    case DeliteCollectionUpdate(a,i,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DeliteCollectionApply(a,i) => Nil
    case DeliteCollectionUpdate(a,i,x) => syms(a)
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

  // TODO: this usage of getBlockResult is ad-hoc and error prone. we need a better way of handling syms that might
  // have come from a reified block.
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case DeliteCollectionSize(x) => emitValDef(sym, quote(x) + ".size")
      case DeliteCollectionApply(x,n) => emitValDef(sym, quote(getBlockResult(x)) + ".dcApply(" + quote(n) + ")")
      case DeliteCollectionUpdate(x,n,y) => emitValDef(sym, quote(getBlockResult(x)) + ".dcUpdate(" + quote(n) + "," + quote(getBlockResult(y)) + ")")
      case _ => super.emitNode(sym, rhs)
    }

  }
}