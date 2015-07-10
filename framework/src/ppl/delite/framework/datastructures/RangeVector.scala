package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.{SourceContext, RefinedManifest}

import ppl.delite.framework.ops._
import ppl.delite.framework.Util._

trait RangeVector
trait UnitRangeVector extends RangeVector  // TBD: Needed?

trait RangeVectorOps extends StructOps {
  implicit def intToUnitRange(x: Rep[Int]): Rep[UnitRangeVector] = delite_unit_range(i)

  object RangeVector {
    def apply(start: Rep[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[RangeVector] = delite_range_vector(start, stride, length)
    def apply(start: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[RangeVector] = delite_range_vector(start, unit(1), length)
  }

  implicit def rangeVectorToRangeOpsCls(x: Rep[RangeVector])(implicit ctx: SourceContext) = new RangeVectorOpsCls(x)
  class RangeVectorOpsCls(x: Rep[RangeVector])(implicit ctx: SourceContext) {
    def start: Rep[Int] = field[Int](x, "start")
    def stride: Rep[Int] = field[Int](x, "stride")
    def length: Rep[Int] = field[Int](x, "length")
  }

  def delite_range_vector(start: Rep[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[RangeVector]
  def delite_unit_range(i: Rep[Int])(implicit ctx: SourceContext): Rep[UnitRangeVector]
}

trait RangeVectorOpsExp extends RangeVectorOps with EffectExp { this: DeliteOpsExp =>
  case class RangeVectorNew(start: Exp[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[RangeVector] {
    val elems = copyTransformedElems(List("start" -> start, "stride" -> stride, "length" -> length))
  }
  case class UnitRangeVectorNew(i: Exp[Int])(implicit ctx: SourceContext) extends DeliteStruct[UnitRangeVector] {
    val elems = copyTransformedElems(List("start" -> i, "stride" -> unit(1), "length" -> unit(1)))
  }

  def delite_range_vector(start: Exp[Int], stride: Exp[Int], length: Exp[Int])(implicit ctx: SourceContext): Exp[RangeVector] = RangeVectorNew(start, stride, length)
  def delite_unit_range(i: Exp[Int])(implicit ctx: SourceContext): Exp[UnitRangeVector] = UnitRangeVectorNew(i)

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if isSubtype(t.erasure,classOf[RangeVector]) => Some((classTag(t), List("start" -> manifest[Int], "stride" -> manifest[Int], "length" -> manifest[Int])))
    case _ => super.unapplyStructType
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@RangeVectorNew(o,s,l) => reflectPure(new {override val original = Some(f,e) } with RangeVectorNew(f(o),f(s),f(l)))(mtype(manifest[A]),ctx)
    case e@UnitRangeVectorNew(i) => reflectPure(new {override val original = Some(f,e) } with UnitRangeVectorNew(f(i)))(mtype(manifest[A]),ctx)
    
    case Reflect(e@RangeVectorNew(o,s,l),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with RangeVectorNew(f(o),f(s),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@UnitRangeVectorNew(i),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with UnitRangeVectorNew(f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}
