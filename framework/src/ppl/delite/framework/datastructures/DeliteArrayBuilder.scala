package ppl.delite.framework.datastructures

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common._
import java.io.PrintWriter
import reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.Config


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
  
  //TODO: extract the length as a field and update it
  case class SoaBuilderTag[T](base: StructTag[T]) extends StructTag[DeliteArrayBuilder[T]]

  //forwarder to appease type-checker
  private def dnew[T:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuilder[T]] = darray_builder_new(initSize)

  private def argManifest[A,B](m: Manifest[A]): Manifest[B] = m.typeArguments(0).asInstanceOf[Manifest[B]]

  override def darray_builder_new[T:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext) = manifest[T] match {
    case rm: RefinedManifest[T] if Config.soaEnabled => 
      struct[DeliteArrayBuilder[T]](SoaBuilderTag(AnonTag(rm)), rm.fields.map(p=>(p._1,dnew(initSize)(p._2,ctx))))
    case _ => super.darray_builder_new(initSize)
  }

  override def darray_builder_plus_equals[T:Manifest](b: Exp[DeliteArrayBuilder[T]], elem: Exp[T])(implicit ctx: SourceContext) = b match {
    case Def(Struct(SoaBuilderTag(tag), elems:Seq[(String, Exp[DeliteArrayBuilder[a]])])) =>
      elems.foreach(p=>darray_builder_plus_equals(p._2, field[a](elem,p._1)(argManifest(p._2.tp),ctx))(argManifest(p._2.tp),ctx))
    case Def(Reflect(Struct(SoaBuilderTag(tag), elems:Seq[(String, Exp[DeliteArrayBuilder[a]])]), u, es)) =>
      elems.foreach(p=>darray_builder_plus_equals(p._2, field[a](elem,p._1)(argManifest(p._2.tp),ctx))(argManifest(p._2.tp),ctx))
    case _ => 
      super.darray_builder_plus_equals(b, elem)
  }

  override def darray_builder_result[T:Manifest](b: Exp[DeliteArrayBuilder[T]])(implicit ctx: SourceContext) = b match {
    case Def(Struct(SoaBuilderTag(tag), elems: Seq[(String, Exp[DeliteArrayBuilder[a]])])) =>
      val arrays = elems.map(p=>(p._1, darray_builder_result(p._2)(argManifest(p._2.tp),ctx)))
      val len = darray_length(arrays.head._2)(argManifest(arrays.head._2.tp),ctx) //which length?
      struct[DeliteArray[T]](SoaTag(tag, len), arrays)
    case Def(Reflect(Struct(SoaBuilderTag(tag), elems: Seq[(String, Exp[DeliteArrayBuilder[a]])]), u, es)) =>
      val arrays = elems.map(p=>(p._1, darray_builder_result(p._2)(argManifest(p._2.tp),ctx)))
      val len = darray_length(arrays.head._2)(argManifest(arrays.head._2.tp),ctx) //which length?
      struct[DeliteArray[T]](SoaTag(tag, len), arrays)
    case _ => super.darray_builder_result(b)
  }
  
}

trait ScalaGenDeliteArrayBuilderOps extends ScalaGenEffect {
  val IR: DeliteArrayBuilderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteArrayBuilderNew(initSize) =>
      emitValDef(sym, "new scala.collection.mutable.ArrayBuffer[" + remap(sym.tp.typeArguments(0)) + "](" + quote(initSize) + ")")
    case DeliteArrayBuilderPlusEquals(b, elem) =>
      emitValDef(sym, quote(b) + " += " + quote(elem))
    case DeliteArrayBuilderResult(b) =>
      emitValDef(sym, quote(b) + ".toArray")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArrayBuilder" => m.typeArguments(0) match {
      case s if s <:< manifest[Record] =>
        structName(m)
      case arg => "scala.collection.mutable.ArrayBuffer[" + remap(arg) + "]"
    }
    case _ => super.remap(m)
  }

}
