package ppl.delite.framework.ops

import scala.reflect.SourceContext
import scala.virtualization.lms.common.{Base, AtomicWrites}
import scala.virtualization.lms.internal.FatExpressions

import ppl.delite.framework.transform.TunnelingTransformer
import ppl.delite.framework.datastructures.DeliteStructsExp

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

trait DeliteAbstractDefsExp extends AtomicWrites with FatExpressions { self =>
  case class AbstractFamily(name: String, var skip: Boolean = false)

  trait AbstractNode[A] extends Def[A] {
    val fm: AbstractFamily
    def family = fm.name
  }

  abstract class AbstractDef[A](implicit fc: AbstractFamily) extends AbstractNode[A] { val fm = fc }
  abstract class AbstractDef2[A:Manifest, R:Manifest](implicit fc: AbstractFamily) extends AbstractDef[R] {
    val mA = manifest[A]
    val mR = manifest[R]
  }
  abstract class AbstractDef3[A:Manifest, B:Manifest, R:Manifest](implicit fc: AbstractFamily) extends AbstractDef2[A,R] {
    val mB = manifest[B]
  }

  abstract class AbstractAtomicWrite[A:Manifest](implicit fc: AbstractFamily) extends AbstractNode[Unit] with AtomicWrite[A] {
    val fm = fc
    val mA = manifest[A]
  }
  abstract class AbstractAtomicTracer(implicit fc: AbstractFamily) extends AtomicTracer {
    def family = fc.name
  }

  def lower[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    sys.error(s"Don't know how to lower $e")
  }

  // TODO: How to quickly check for convergence here?
  abstract class Implementer(implicit val fc: AbstractFamily) extends TunnelingTransformer {
    val IR: self.type

    // Prior to beginning implementation, set abstract family to "skip" abstract implementation
    // when possible, instead directly generating the implementation. Nodes are not required to
    // honor this setting, but it can potentially reduce the number of transformation passes
    // Note that convergence is not generally guaranteed if an implementer can potentially generate
    // nodes in its own abstract family
    override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
      fc.skip = true
      super.preprocess(b)
    }

    // TODO: Change abstract family matching to pointer matching rather than string matching?
    // Would require the family definition to be visible to both the nodes and transformer
    override def transformTP[A](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = rhs match {
      case d: AbstractNode[_] if d.family == fc.name =>
        printdbg("Transforming abstract node in family " + d.family)
        Some(lower(rhs, self.asInstanceOf[Transformer])(mtype(lhs.tp), ctx))

      case d: AbstractNode[_] =>
        printdbg("Ignoring abstract node in family " + d.family + " ( != " + fc.name + ")")
        None

      case _ => super.transformTP(lhs, rhs)
    }
  }
}

trait DeliteAbstractOpsExp extends DeliteAbstractDefsExp with AbstractIndicesOpsExp { this: DeliteOpsExp =>

  // TODO: Should one inherit from the other? Should they have a common parent besides DeliteOp and AbstractNode?
  abstract class DeliteAbstractOp[R:Manifest](implicit fc: AbstractFamily) extends DeliteOp[R] with AbstractNode[R] {
    type OpType <: DeliteAbstractOp[R]
    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
    val mR = manifest[R]
    val fm = fc
  }
  abstract class DeliteAbstractOp2[A:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractOp[R] {
    type OpType <: DeliteAbstractOp2[A,R]
    val mA = manifest[A]
  }
  abstract class DeliteAbstractOp3[A:Manifest,B:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractOp2[A,R] {
    type OpType <: DeliteAbstractOp3[A,B,R]
    val mB = manifest[B]
  }
  abstract class DeliteAbstractOp4[A:Manifest,B:Manifest,C:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractOp3[A,B,R] {
    type OpType <: DeliteAbstractOp4[A,B,C,R]
    val mC = manifest[C]
  }

  abstract class DeliteAbstractNestedOp[R:Manifest](implicit fc: AbstractFamily) extends DeliteOp[R] with AbstractNode[R] {
    type OpType <: DeliteAbstractNestedOp[R]
    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
    val vs: Sym[LoopIndices] = copyOrElse(_.vs)(loopIndicesEmpty(v).asInstanceOf[Sym[LoopIndices]])
    val mR = manifest[R]
    val fm = fc
  }
  abstract class DeliteAbstractNestedOp2[A:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractNestedOp[R] {
    type OpType <: DeliteAbstractNestedOp2[A,R]
    val mA = manifest[A]
  }
  abstract class DeliteAbstractNestedOp3[A:Manifest,B:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractNestedOp2[A,R] {
    type OpType <: DeliteAbstractNestedOp3[A,B,R]
    val mB = manifest[B]
  }
  abstract class DeliteAbstractNestedOp4[A:Manifest,B:Manifest,C:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractOp3[A,B,R] {
    type OpType <: DeliteAbstractNestedOp4[A,B,C,R]
    val mC = manifest[C]
  }
}
