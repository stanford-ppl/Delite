/*package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.{SourceContext, RefinedManifest}

import ppl.delite.framework.ops._
import ppl.delite.framework.Util._

trait DeliteIndices
trait DeliteIndexVector extends DeliteIndices
trait DeliteIndexRange extends DeliteIndices

trait DeliteIndicesOps extends StructOps {
  object DeliteIndexRange {
    def apply(start: Rep[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteIndexRange] = dirange_new(start, stride, length)
    def apply(start: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteIndexRange] = dirange_new(start, unit(1), length)
  }

  implicit def indexRangeToRangeOps(x: Rep[DeliteIndexRange])(implicit ctx: SourceContext) = new IndexRangeOpsCls(x)
  class IndexRangeOpsCls(x: Rep[DeliteIndexRange])(implicit ctx: SourceContext) {
    def start: Rep[Int] = dirange_start(x)
    def stride: Rep[Int] = dirange_stride(x)
    def length: Rep[Int] = dirange_length(x)
    def len: Rep[Int] = dirange_length(x)
  }

  def dirange_new(start: Rep[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteIndexRange]

  def dirange_start(x: Rep[DeliteIndexRange])(implicit ctx: SourceContext): Rep[Int] = field[Int](x, "start")
  def dirange_stride(x: Rep[DeliteIndexRange])(implicit ctx: SourceContext): Rep[Int] = field[Int](x, "stride")
  def dirange_length(x: Rep[DeliteIndexRange])(implicit ctx: SourceContext): Rep[Int] = field[Int](x, "length")
}

trait DeliteIndicesOpsExp extends DeliteIndicesOps with DeliteStructsExp { this: DeliteOpsExp =>

  case class DeliteIndexRangeNew(start: Exp[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[DeliteIndexRange] {
    val elems = copyTransformedElems(List("start" -> start, "stride" -> stride, "length" -> length))
  }

  def dirange_new(start: Exp[Int], stride: Exp[Int], length: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteIndexRange] = DeliteIndexRangeNew(start, stride, length)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteIndexRangeNew(o,s,l) => reflectPure(new {override val original = Some(f,e) } with DeliteIndexRangeNew(f(o),f(s),f(l)))(mtype(manifest[A]),ctx)
    case Reflect(e@DeliteIndexRangeNew(o,s,l),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with DeliteIndexRangeNew(f(o),f(s),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if isSubtype(t.erasure,classOf[DeliteIndexRange]) => Some((classTag(t), List("start" -> manifest[Int], "stride" -> manifest[Int], "length" -> manifest[Int])))
    case _ => super.unapplyStructType
  }
}*/