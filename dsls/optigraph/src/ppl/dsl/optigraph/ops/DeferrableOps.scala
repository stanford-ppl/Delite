package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait DeferrableOps extends Variables {
  this: OptiGraph =>
  
  /** Deferrable constructor */
  object Deferrable {
    /** Creates a new Deferrable with initial value init */
    def apply[T:Manifest](init:Rep[T]) = def_new(init)
  }
    
  implicit def repDeferrableToDeferrableOps[T:Manifest](d: Rep[Deferrable[T]]) = new DeferrableOpsCls(d)
  // should implicitly convert a Deferrable object to its value s.t. the '.value' doesn't have to be explicitly called
  implicit def repDeferrableToVal[T:Manifest](d: Rep[Deferrable[T]]):Rep[T] = d.value
  
  /** Operations on Deferrables */
  class DeferrableOpsCls[T:Manifest](d: Rep[Deferrable[T]]) {
    /** Assign the latest deferred value
     *  No effect if no value was deferred since last assignment */
    def assign(): Rep[Unit] = def_assign(d)
    /** Get the current value (latest value assigned) */
    def value: Rep[T] = def_getvalue(d)
    /** Set the current value */
    def setValue(v: Rep[T]): Rep[Unit] = def_setvalue(d, v)
    /** Defer a value assignment */
    def <=(v: Rep[T]): Rep[Unit] = def_defer(d, v)
  }
  
  /** Compares current values only (i.e. the latest values already assigned) */
  def __equals[T:Manifest](d1: Rep[Deferrable[T]], d2: Rep[Deferrable[T]]): Rep[Boolean]
  def def_new[T:Manifest](init: Rep[T]): Rep[Deferrable[T]]
  def def_assign[T:Manifest](d: Rep[Deferrable[T]]): Rep[Unit]
  def def_getvalue[T:Manifest](d: Rep[Deferrable[T]]): Rep[T]
  def def_setvalue[T:Manifest](d: Rep[Deferrable[T]], v: Rep[T]): Rep[Unit]
  def def_defer[T:Manifest](d: Rep[Deferrable[T]], v: Rep[T]): Rep[Unit]
}

trait DeferrableOpsExp extends DeferrableOps with EffectExp {
  this: OptiGraphExp =>
  
  case class DefObjectNew[T](init: Rep[T])(val mD: Manifest[Deferrable[T]]) extends Def[Deferrable[T]]
  case class DefAssign[T:Manifest](d: Exp[Deferrable[T]]) extends Def[Unit]
  case class DefGetValue[T:Manifest](d: Exp[Deferrable[T]]) extends Def[T]
  case class DefSetValue[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) extends Def[Unit]
  case class DefDefer[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) extends Def[Unit]
  
  def def_new[T:Manifest](init: Exp[T]) = reflectMutable(DefObjectNew(init)(manifest[Deferrable[T]]))
  def def_assign[T:Manifest](d: Exp[Deferrable[T]]) = reflectWrite(d)(DefAssign(d))
  def def_getvalue[T:Manifest](d: Exp[Deferrable[T]]) = reflectPure(DefGetValue(d))
  def def_setvalue[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) = reflectWrite(d)(DefSetValue(d, v))
  def def_defer[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) = reflectWrite(d)(DefDefer(d,v))
  def __equals[T:Manifest](d1: Exp[Deferrable[T]], d2: Exp[Deferrable[T]]) = {
    d1.value == d2.value
  }
}

trait BaseGenDeferrableOps extends GenericNestedCodegen {
  val IR: DeferrableOpsExp
  import IR._

}

trait ScalaGenDeferrableOps extends BaseGenDeferrableOps with ScalaGenBase {
  val IR: DeferrableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case d@DefObjectNew(init) => emitValDef(sym, "new " + remap(d.mD) + "(" + quote(init) +")")
      case DefAssign(d) => emitValDef(sym, quote(d) + ".assign")
      case DefGetValue(d) => emitValDef(sym, quote(d) + ".value")
      case DefSetValue(d,v) => emitValDef(sym, quote(d) + ".setValue("+ quote(v) + ")")
      case DefDefer(d,v) => emitValDef(sym, quote(d) + ".defer(" + quote(v) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

