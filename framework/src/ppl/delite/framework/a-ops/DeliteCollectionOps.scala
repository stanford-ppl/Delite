package ppl.delite.framework.ops

import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenEffect}
import scala.virtualization.lms.internal.{GenericNestedCodegen}

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

trait DeliteCollectionOpsExp extends DeliteCollectionOps with EffectExp {
  case class DeliteCollectionSize[A:Manifest](x: Exp[DeliteCollection[A]]) extends Def[Int]
  case class DeliteCollectionApply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) extends Def[A]
  case class DeliteCollectionUpdate[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]

  def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]) = DeliteCollectionSize(x)
  def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = DeliteCollectionApply(x,n) // reflectRead(x)
  def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) = reflectWrite(x)(DeliteCollectionUpdate(x,n,y)) // reflectWrite(x)

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case DeliteCollectionApply(x, n) => dc_apply(f(x), f(n))
    case Reflect(DeliteCollectionApply(l,r), u, es) => reflectMirrored(Reflect(DeliteCollectionApply(f(l),f(r)), mapOver(f,u), f(es)))    
    case _ => super.mirror(e, f)
  }
  
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

trait BaseGenDeliteCollectionOps extends GenericNestedCodegen {
  val IR: DeliteCollectionOpsExp
  import IR._

}
trait ScalaGenDeliteCollectionOps extends BaseGenDeliteCollectionOps with ScalaGenEffect {
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