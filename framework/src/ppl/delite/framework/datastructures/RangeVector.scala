package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.{SourceContext, RefinedManifest}

import ppl.delite.framework.ops._
import ppl.delite.framework.Util._

trait RangeVector
trait RangeWildcard extends RangeVector

trait RangeVectorOps extends StructOps {
  object RangeVector {
    def apply(start: Rep[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[RangeVector] = delite_range_vector(start, stride, length)
    def apply(start: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[RangeVector] = delite_range_vector(start, unit(1), length)
  }

  // Bit of a hack here to get RangeWildcard to be able to be used in the same places RangeVector can be
  implicit def rangeVectorToRangeOpsCls(x: Rep[RangeVector])(implicit ctx: SourceContext) = new RangeVectorOpsCls(x)
  class RangeVectorOpsCls(x: Rep[RangeVector])(implicit ctx: SourceContext) {
    def start: Rep[Int] = range_vector_start(x)
    def stride: Rep[Int] = range_vector_stride(x)
    def length: Rep[Int] = range_vector_length(x)
    def len: Rep[Int] = range_vector_length(x)
    def length(lenCtx: Rep[Int]): Rep[Int] = range_vector_length_with_ctx(x, lenCtx)
  }

  def range_vector_start(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int]
  def range_vector_stride(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int]
  def range_vector_length(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int]
  def range_vector_length_with_ctx(x: Rep[RangeVector], lenCtx: Rep[Int])(implicit ctx: SourceContext): Rep[Int]

  def delite_range_vector(start: Rep[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext): Rep[RangeVector]
  //def delite_unit_range(i: Rep[Int])(implicit ctx: SourceContext): Rep[UnitRangeVector]
}

trait RangeVectorOpsExp extends RangeVectorOps with DeliteStructsExp { this: DeliteOpsExp =>
  
  def range_vector_start(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int] = {
    if ( isSubtype(x.tp.erasure, classOf[RangeWildcard]) ) { unit(0) }
    else field[Int](x, "start") 
  }
  def range_vector_stride(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int] = {
    if ( isSubtype(x.tp.erasure, classOf[RangeWildcard]) ) { unit(1) }
    else field[Int](x, "stride")
  }
  def range_vector_length(x: Rep[RangeVector])(implicit ctx: SourceContext): Rep[Int] = {
    if ( isSubtype(x.tp.erasure, classOf[RangeWildcard]) ) { sys.error("No length context given to RangeWildcard") }
    else field[Int](x, "length")
  }
  def range_vector_length_with_ctx(x: Rep[RangeVector], lenCtx: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = {
    if ( isSubtype(x.tp.erasure, classOf[RangeWildcard]) ) { lenCtx }
    else field[Int](x, "length")
  }


  case class RangeVectorNew(start: Exp[Int], stride: Rep[Int], length: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[RangeVector] {
    val elems = copyTransformedElems(List("start" -> start, "stride" -> stride, "length" -> length))
  }
  /*case class UnitRangeVectorNew(i: Exp[Int])(implicit ctx: SourceContext) extends DeliteStruct[UnitRangeVector] {
    val elems = copyTransformedElems(List("start" -> i, "stride" -> unit(1), "length" -> unit(1)))
  }*/

  def delite_range_vector(start: Exp[Int], stride: Exp[Int], length: Exp[Int])(implicit ctx: SourceContext): Exp[RangeVector] = RangeVectorNew(start, stride, length)
  //def delite_unit_range(i: Exp[Int])(implicit ctx: SourceContext): Exp[UnitRangeVector] = UnitRangeVectorNew(i)

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if isSubtype(t.erasure,classOf[RangeVector]) => Some((classTag(t), List("start" -> manifest[Int], "stride" -> manifest[Int], "length" -> manifest[Int])))
    case _ => super.unapplyStructType
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@RangeVectorNew(o,s,l) => reflectPure(new {override val original = Some(f,e) } with RangeVectorNew(f(o),f(s),f(l)))(mtype(manifest[A]),ctx)
    //case e@UnitRangeVectorNew(i) => reflectPure(new {override val original = Some(f,e) } with UnitRangeVectorNew(f(i)))(mtype(manifest[A]),ctx)
    
    case Reflect(e@RangeVectorNew(o,s,l),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with RangeVectorNew(f(o),f(s),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    //case Reflect(e@UnitRangeVectorNew(i),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with UnitRangeVectorNew(f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}
