package ppl.delite.framework.datastructures

import scala.reflect.SourceContext
import scala.virtualization.lms.common.Base
import ppl.delite.framework.ops.DeliteOpsExp

trait AbstractIndices
trait Indices extends AbstractIndices
trait LoopIndices extends AbstractIndices

trait AbstractIndicesOps extends Base {
  object Indices {
    def apply(xs: Rep[Int]*) = indices_new(xs.toList)
  }

  implicit def repIndicesToIndicesOpsCls(x: Rep[AbstractIndices]) = new IndicesOpsCls(x)
  class IndicesOpsCls(x: Rep[AbstractIndices]) {
    def apply(i: Int)  = indices_apply(x, i)
  }
  implicit def repLoopIndicesToLoopIndicesOpsCls(x: Rep[LoopIndices]) = new LoopIndicesOpsCls(x)
  class LoopIndicesOpsCls(x: Rep[LoopIndices]) {
    def flat = loopindices_flat(x)
  }

  def indices_apply(s: Rep[AbstractIndices], i: Int): Rep[Int]
  def indices_new(xs: List[Rep[Int]]): Rep[Indices]
  def loopindices_flat(x: Rep[LoopIndices]): Rep[Int]
}

trait AbstractIndicesOpsExp extends AbstractIndicesOps with DeliteStructsExp { this: DeliteOpsExp =>

  case class IndicesNew(xs: List[Exp[Int]]) extends DeliteStruct[Indices] {
    val elems = copyTransformedElems(xs.zipWithIndex.map{i => "i"+i._2 -> i._1})
  }
  case class LoopIndicesNew(flat: Exp[Int], xs: List[Exp[Int]]) extends DeliteStruct[LoopIndices] {
    val elems = copyTransformedElems( ("flat" -> flat) +: xs.zipWithIndex.map{i => "i"+i._2 -> i._1})
  }

  def loopIndicesEmpty(flat: Exp[Int]) = reflectPure(LoopIndicesNew(flat, Nil))
  def loopIndices(flat: Exp[Int], xs: List[Exp[Int]]) = reflectPure(LoopIndicesNew(flat, xs))

  def indices_apply(x: Exp[AbstractIndices], i: Int) = field[Int](x, "i" + i)
  def indices_new(x: List[Exp[Int]]) = reflectPure(IndicesNew(x))
  def loopindices_flat(x: Exp[LoopIndices]) = field[Int](x, "flat")

  // HACK: Allow index apply of LoopIndices before it's available
  override def field[T:Manifest](struct: Exp[Any], index: String)(implicit pos: SourceContext): Exp[T] = struct match {
    case Def(LoopIndicesNew(f,i)) if i.isEmpty => reflectPure(FieldApply[T](struct,index))
    case _ => super.field[T](struct, index)
  }

  // HACK: Unknown number of fields in structs - can't give full elems list here...
  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if isSubtype(t.erasure,classOf[AbstractIndices]) => Some((classTag(t), Nil))
    case _ => super.unapplyStructType
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@IndicesNew(x) => reflectPure(new {override val original = Some(f,e) } with IndicesNew(f(x)))(mtype(manifest[A]),ctx)
    case e@LoopIndicesNew(l,i) => reflectPure(new {override val original = Some(f,e) } with LoopIndicesNew(f(l),f(i)))(mtype(manifest[A]),ctx)

    case Reflect(e@IndicesNew(x),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with IndicesNew(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@LoopIndicesNew(l,i),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with LoopIndicesNew(f(l),f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}
