package ppl.delite.framework.ops

import ppl.delite.framework.datastructures._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import ppl.delite.framework.Util._

// Abstract placeholder for indices
// These should eventually all be optimized away
trait AbstractIndices
trait Indices extends AbstractIndices
trait LoopIndices extends AbstractIndices

trait IndicesOps extends Base {
  object Indices { def apply(xs: Rep[Int]*) = indices_new(xs.toList) }

  implicit def repIndicesToIndicesOpsCls(x: Rep[AbstractIndices]) = new IndicesOpsCls(x)
  class IndicesOpsCls(x: Rep[AbstractIndices]) { def apply(i: Int) = indices_apply(x, i) }

  implicit def loopindicesToOps(x: Rep[LoopIndices]) = new LoopIndicesOpsCls(x) 
  class LoopIndicesOpsCls(x: Rep[LoopIndices]) { def flat = loopindices_flat(x) }

  def indices_new(xs: Seq[Rep[Int]]): Rep[Indices]
  def indices_apply(s: Rep[AbstractIndices], i: Int): Rep[Int]
  def loopindices_flat(x: Rep[LoopIndices]): Rep[Int]
}

trait IndicesOpsExp extends IndicesOps with DeliteStructsExp {
  this: DeliteOpsExp => // Needed for DeliteStructsExp

  case class IndicesNew(xs: List[Exp[Int]]) extends DeliteStruct[Indices] {
    val elems = copyTransformedElems(xs.zipWithIndex.map{i => ("i" + i._2) -> i._1})
  }
  case class LoopIndicesNew(flat: Exp[Int], is: Seq[Exp[Int]], ds: Seq[Exp[Int]]) extends DeliteStruct[LoopIndices] {
    val elems = copyTransformedElems( (("flat" -> flat) +: is.zipWithIndex.map{i => ("i" + i._2) -> i._1}) ++ ds.zipWithIndex.map{i => ("d" + i._2) -> i._1})
  }

  // Should NOT have Reflect of abstract indices - this can pollute the context, causing these nodes to be preserved even when they shouldn't be
  def loopIndices(i: Exp[Int]): Exp[LoopIndices] = reflectPure(LoopIndicesNew(i, Nil, Nil))
  def loopindices_new(flat: Exp[Int], inds: Seq[Exp[Int]], dims: Seq[Exp[Int]]): Exp[LoopIndices] = reflectPure(LoopIndicesNew(flat,inds,dims))

  def indices_new(x: Seq[Exp[Int]]): Exp[Indices] = reflectPure(IndicesNew(x.toList))
  def indices_apply(x: Exp[AbstractIndices], i: Int): Exp[Int] = field[Int](x, "i" + i)
  def loopindices_flat(x: Exp[LoopIndices]): Exp[Int] = field[Int](x, "flat")

  // HACK: Allow index apply of LoopIndices before it's available
  override def field[T:Manifest](struct: Exp[Any], index: String)(implicit pos: SourceContext): Exp[T] = struct match {
    case Def(LoopIndicesNew(f,i,d)) if index != "flat" && i.isEmpty => reflectPure(FieldApply[T](struct,index))
    case _ => super.field[T](struct,index)
  }

  // HACK: unknown number of fields in struct - can't give full elems list here...
  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if isSubtype(t.erasure,classOf[AbstractIndices]) => Some((classTag(t), Nil))
    case _ => super.unapplyStructType
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@IndicesNew(x) => reflectPure(new {override val original = Some(f,e) } with IndicesNew(f(x)))(mtype(manifest[A]),ctx)
    case e@LoopIndicesNew(l,i,d) => reflectPure(new {override val original = Some(f,e) } with LoopIndicesNew(f(l),f(i),f(d)))(mtype(manifest[A]),ctx)
    
    case Reflect(e@IndicesNew(x),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with IndicesNew(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@LoopIndicesNew(l,i,d),u,es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with LoopIndicesNew(f(l),f(i),f(d)), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait DeliteAbstractOps extends IndicesOps

/**
 * Base trait for creating nodes with no codegen rules. 
 * Extending trait should look something like:
 *
 * trait FooOpsExp extends DriftOpsExp {
 *   private implicit val fc = AbstractFamily("foo")
 *   case class Foo(...) extends DriftDef[...]   // can optionally use DriftDef[...](fc) here, but not needed
 *   ...
 * }
 *
 */
trait DeliteAbstractOpsExp extends DeliteAbstractOps with DeliteOpsExpIR with AtomicWriteExp with IndicesOpsExp {
  this: DeliteOpsExp => // Needed for DeliteStructsExp

  case class AbstractFamily(name: String) 

  /**
   * Base class for nodes which require transformation before codegen
   */
  trait AbstractNode[A] extends Def[A] { 
    val fm: AbstractFamily
    def family = fm.name 
  }

  abstract class AbstractDef[A](implicit fc: AbstractFamily) extends AbstractNode[A] { val fm = fc }
  abstract class AbstractDefWithManifest[A:Manifest, R:Manifest](implicit fc: AbstractFamily) extends AbstractDef[R] {
    val mA = manifest[A]
    val mR = manifest[R]
  }

  abstract class AbstractAtomicWriteDef[A:Manifest](implicit fc: AbstractFamily) extends AbstractNode[Unit] with AtomicWrite[A] { val fm = fc; val mA = manifest[A] } 
  abstract class AbstractAtomicTracer(implicit fc: AbstractFamily) extends AtomicTracer { def family = fc.name }

  /** 
   * Base class for multi-loops that require transformation before codegen
   * TODO: Should this extend AbstractLoop in LMS? Would need a 'size' field
   */
  abstract class DeliteAbstractLoop[A:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteOp[R] with AbstractNode[R] { 
    type OpType <: DeliteAbstractLoop[A,R]
    lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val i: Sym[LoopIndices] = copyTransformedOrElse(_.i)(loopIndices(v)).asInstanceOf[Sym[LoopIndices]]
    val mA = manifest[A]
    val mR = manifest[R]
    val fm = fc
  }
  abstract class DeliteAbstractLoop2[A:Manifest,B:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractLoop[A,R] { val mB = manifest[B] }
  abstract class DeliteAbstractLoop3[A:Manifest,B:Manifest,T:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractLoop2[A,B,R] { val mT = manifest[T] }
}

