package ppl.delite.framework.datastructures

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common._
import java.io.PrintWriter
import reflect.{SourceContext, RefinedManifest}


/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

trait DeliteArrayBuilderOps extends Base { this: DeliteArrayOps =>

  trait DeliteArrayBuilder[T]

  object DeliteArrayBuilder {
    def apply[T:Manifest]()(implicit ctx: SourceContext) = darray_builder_new(unit(16))
    def apply[T:Manifest](initSize: Rep[Int])(implicit ctx: SourceContext) = darray_builder_new(initSize)
  }

  implicit def repDArrayBuilderToDArrayBuilderOps[T:Manifest](b: Rep[DeliteArrayBuilder[T]]) = new DeliteArrayBuilderOpsCls(b)

  class DeliteArrayBuilderOpsCls[T:Manifest](b: Rep[DeliteArrayBuilder[T]]) {
    def +=(elem: Rep[T])(implicit ctx: SourceContext) = darray_builder_plus_equals(b,elem)
    def result(implicit ctx: SourceContext) = darray_builder_result(b)
  }

  def darray_builder_new[T:Manifest](initSize: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuilder[T]]
  def darray_builder_plus_equals[T:Manifest](b: Rep[DeliteArrayBuilder[T]], elem: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def darray_builder_result[T:Manifest](b: Rep[DeliteArrayBuilder[T]])(implicit ctx: SourceContext): Rep[DeliteArray[T]]


}

trait DeliteArrayBuilderOpsExp extends DeliteArrayBuilderOps with StructExp with EffectExp {
  this: DeliteArrayOpsExp with DeliteOpsExp =>

  //ideally we could implement as a struct wrapper of DeliteArray and handle resizing, etc in IR,
  //but += operation seems to require a Var(mutable Array)

  case class DeliteArrayBuilderNew[T:Manifest](initSize: Exp[Int]) extends Def[DeliteArrayBuilder[T]]
  case class DeliteArrayBuilderPlusEquals[T:Manifest](b: Exp[DeliteArrayBuilder[T]], elem: Exp[T]) extends Def[Unit]
  case class DeliteArrayBuilderResult[T:Manifest](b: Exp[DeliteArrayBuilder[T]]) extends Def[DeliteArray[T]]

  def darray_builder_new[T:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(DeliteArrayBuilderNew(initSize))
  def darray_builder_plus_equals[T:Manifest](b: Exp[DeliteArrayBuilder[T]], elem: Exp[T])(implicit ctx: SourceContext) = reflectWrite(b)(DeliteArrayBuilderPlusEquals(b,elem))
  def darray_builder_result[T:Manifest](b: Exp[DeliteArrayBuilder[T]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBuilderResult(b))

}

trait DeliteArrayBuilderOpsExpOpt extends DeliteArrayBuilderOpsExp with StructExpOptCommon {
  this: DeliteArrayOpsExpOpt with DeliteOpsExp =>
  
  //forwarder to appease type-checker
  private def dnew[T:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuilder[T]] = darray_builder_new(initSize)

  private def argManifest[A,B](m: Manifest[A]): Manifest[B] = m.typeArguments(0).asInstanceOf[Manifest[B]]

  override def darray_builder_new[T:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext) = manifest[T] match {
    case rm: RefinedManifest[T] => struct[DeliteArrayBuilder[T]](rm.fields.map(p=>(p._1,dnew(initSize)(p._2,ctx))):_*)
    case _ => super.darray_builder_new(initSize)
  }

  override def darray_builder_plus_equals[T:Manifest](b: Exp[DeliteArrayBuilder[T]], elem: Exp[T])(implicit ctx: SourceContext) = b match {
    case Def(Struct(tag, elems:Map[String, Exp[DeliteArrayBuilder[a]]])) =>
      elems.foreach(p=>darray_builder_plus_equals(p._2, field[a](elem.asInstanceOf[Exp[Record]],p._1)(argManifest(p._2.Type),ctx))(argManifest(p._2.Type),ctx))
    case _ => super.darray_builder_plus_equals(b, elem)
  }

  override def darray_builder_result[T:Manifest](b: Exp[DeliteArrayBuilder[T]])(implicit ctx: SourceContext) = b match {
    case Def(Struct(tag, elems: Map[String, Exp[DeliteArrayBuilder[a]]])) =>
      struct[DeliteArray[T]](tag, elems.map(p=>(p._1, darray_builder_result(p._2)(argManifest(p._2.Type),ctx))))
    case _ => super.darray_builder_result(b)
  }
  
}

trait ScalaGenDeliteArrayBuilderOps extends ScalaGenEffect {
  val IR: DeliteArrayBuilderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DeliteArrayBuilderNew(initSize) =>
      emitValDef(sym, "new scala.collection.mutable.ArrayBuffer[" + remap(sym.Type.typeArguments(0)) + "](" + quote(initSize) + ")")
    case DeliteArrayBuilderPlusEquals(b, elem) =>
      emitValDef(sym, quote(b) + " += " + quote(elem))
    case DeliteArrayBuilderResult(b) =>
      emitValDef(sym, quote(b) + ".toArray")
    case _ => super.emitNode(sym, rhs)
  }

}
