package ppl.delite.framework.ops

import ppl.delite.framework.DeliteCollection
import java.io.PrintWriter
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.internal._

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
  def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = DeliteCollectionApply(reflectRead(x),n)
  def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(DeliteCollectionUpdate(reflectWrite(x),n,y))
}

trait BaseGenDeliteCollectionOps extends GenericNestedCodegen {
  val IR: DeliteCollectionOpsExp
  import IR._

  //override def syms(e: Any): List[Sym[Any]] = e match {
    //case _ => super.syms(e)
  //}
}
trait ScalaGenDeliteCollectionOps extends BaseGenDeliteCollectionOps with ScalaGenEffect {
  val IR: DeliteCollectionOpsExp
  import IR._

  // TODO: this usage of getBlockResult is ad-hoc and error prone. we need a better way of handling syms that might
  // have come from a reified block.
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case DeliteCollectionSize(x) => emitValDef(sym, quote(x) + ".size")
      case DeliteCollectionApply(x,n) => emitValDef(sym, quote(getBlockResult(x)) + ".dcApply(" + quote(n) + ")")
      case DeliteCollectionUpdate(x,n,y) => emitValDef(sym, quote(getBlockResult(x)) + ".dcUpdate(" + quote(n) + "," + quote(getBlockResult(y)) + ")")
      case _ => super.emitNode(sym, rhs)
    }

  }
}