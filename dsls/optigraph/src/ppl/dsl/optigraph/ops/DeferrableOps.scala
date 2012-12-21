package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import reflect.{Manifest,SourceContext}
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
  def def_setdefvalue[T:Manifest](d: Rep[Deferrable[T]], v: Rep[T]): Rep[Unit]
  def def_getdefvalue[T:Manifest](d: Rep[Deferrable[T]]): Rep[T]
  def def_setdeferred[T:Manifest](d: Rep[Deferrable[T]], v: Rep[Boolean]): Rep[Unit]
  def def_getdeferred[T:Manifest](d: Rep[Deferrable[T]]): Rep[Boolean]
}

trait DeferrableOpsExp extends DeferrableOps with EffectExp {
  this: OptiGraphExp =>

  case class DefObjectNew[T](init: Rep[T])(val mD: Manifest[Deferrable[T]]) extends Def[Deferrable[T]]
  case class DefGetValue[T:Manifest](d: Exp[Deferrable[T]]) extends Def[T]
  case class DefSetValue[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) extends Def[Unit]
  case class DefSetDefValue[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) extends Def[Unit]
  case class DefGetDefValue[T:Manifest](d: Exp[Deferrable[T]]) extends Def[T]
  case class DefSetDeferred[T:Manifest](d: Exp[Deferrable[T]], v: Exp[Boolean]) extends Def[Unit]
  case class DefGetDeferred[T:Manifest](d: Exp[Deferrable[T]]) extends Def[Boolean]

  case class DefDefer[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(deferrable_defer_impl(d, v)))

  case class DefAssign[T:Manifest](d: Exp[Deferrable[T]])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(deferrable_assign_impl(d)))

  def def_new[T:Manifest](init: Exp[T]) = reflectMutable(DefObjectNew(init)(manifest[Deferrable[T]]))
  def def_assign[T:Manifest](d: Exp[Deferrable[T]]) = reflectWrite(d)(DefAssign(d))
  def def_getvalue[T:Manifest](d: Exp[Deferrable[T]]) = reflectPure(DefGetValue(d))
  def def_setvalue[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) = reflectWrite(d)(DefSetValue(d, v))
  def def_defer[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) = reflectWrite(d)(DefDefer(d,v))
  def __equals[T:Manifest](d1: Exp[Deferrable[T]], d2: Exp[Deferrable[T]]) = {
    d1.value == d2.value
  }
  def def_setdefvalue[T:Manifest](d: Exp[Deferrable[T]], v: Exp[T]) = reflectWrite(d)(DefSetDefValue(d, v))
  def def_getdefvalue[T:Manifest](d: Exp[Deferrable[T]]) = reflectPure(DefGetDefValue(d))
  def def_setdeferred[T:Manifest](d: Exp[Deferrable[T]], v: Exp[Boolean]) = reflectWrite(d)(DefSetDeferred(d, v))
  def def_getdeferred[T:Manifest](d: Exp[Deferrable[T]]) = reflectPure(DefGetDeferred(d))
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case DefGetValue(d) => def_getvalue(f(d))
    case Reflect(e@DefObjectNew(i), u, es) => reflectMirrored(Reflect(DefObjectNew(f(i))(e.mD), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefSetValue(d,v), u, es) => reflectMirrored(Reflect(DefSetValue(f(d),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefGetValue(d), u, es) => reflectMirrored(Reflect(DefGetValue(f(d)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefAssign(d), u, es) => reflectMirrored(Reflect(DefAssign(f(d)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefDefer(d,v), u, es) => reflectMirrored(Reflect(DefDefer(f(d),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefSetDefValue(d,v), u, es) => reflectMirrored(Reflect(DefSetDefValue(f(d),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefGetDefValue(d), u, es) => reflectMirrored(Reflect(DefGetDefValue(f(d)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefSetDeferred(d,v), u, es) => reflectMirrored(Reflect(DefSetDeferred(f(d),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DefGetDeferred(d), u, es) => reflectMirrored(Reflect(DefGetDeferred(f(d)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BaseGenDeferrableOps extends GenericNestedCodegen {
  val IR: DeferrableOpsExp
  import IR._

}

trait ScalaGenDeferrableOps extends BaseGenDeferrableOps with ScalaGenBase {
  val IR: DeferrableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case d@DefObjectNew(init) => emitValDef(sym, "new " + remap(d.mD) + "(" + quote(init) +")")
      case DefGetValue(d) => emitValDef(sym, quote(d) + ".value")
      case DefSetValue(d,v) => emitValDef(sym, quote(d) + ".setValue("+ quote(v) + ")")
      case DefSetDefValue(d,v) => emitValDef(sym, quote(d) + ".setDefValue("+ quote(v) + ")")
      case DefGetDefValue(d) => emitValDef(sym, quote(d) + ".getDefValue")
      case DefSetDeferred(d,v) => emitValDef(sym, quote(d) + ".setDeferred("+ quote(v) + ")")
      case DefGetDeferred(d) => emitValDef(sym, quote(d) + ".getDeferred")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

