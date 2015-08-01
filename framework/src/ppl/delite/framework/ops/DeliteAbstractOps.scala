package ppl.delite.framework.ops

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._

import ppl.delite.framework.visit._

import ppl.delite.framework.Util._

// TODO: Move this elsewhere
// Abstract placeholder for indices
// These should eventually all be optimized away
trait AbstractIndices
trait Indices extends AbstractIndices
trait LoopIndices extends AbstractIndices

trait IndicesOps extends Base {
  implicit def repIndicesToIndicesOpsCls(x: Rep[AbstractIndices]) = new IndicesOpsCls(x)
  class IndicesOpsCls(x: Rep[AbstractIndices]) { def apply(i: Int) = indices_apply(x, i) }
  def indices_apply(s: Rep[AbstractIndices], i: Int): Rep[Int]

  object Indices { def apply(xs: Rep[Int]*) = indices_new(xs.toList) }
  def indices_new(xs: Seq[Rep[Int]]): Rep[Indices]

  implicit def loopindicesToOps(x: Rep[LoopIndices]) = new LoopIndicesOpsCls(x) 
  class LoopIndicesOpsCls(x: Rep[LoopIndices]) { def flat = loopindices_flat(x) }
  def loopindices_flat(x: Rep[LoopIndices]): Rep[Int]
}

trait IndicesOpsExp extends IndicesOps with DeliteStructsExp { this: DeliteOpsExp =>

  case class IndicesNew(xs: List[Exp[Int]]) extends DeliteStruct[Indices] {
    val elems = copyTransformedElems(xs.zipWithIndex.map{i => ("i" + i._2) -> i._1})
  }
  case class LoopIndicesNew(flat: Exp[Int], is: Seq[Exp[Int]]) extends DeliteStruct[LoopIndices] {
    val elems = copyTransformedElems( ("flat" -> flat) +: is.zipWithIndex.map{i => ("i" + i._2) -> i._1} )
  }

  // Should NOT have Reflect of abstract indices - this can pollute the context, causing these nodes to be preserved even when they shouldn't be
  def loopIndices(flat: Exp[Int]): Exp[LoopIndices] = reflectPure(LoopIndicesNew(flat, Nil))
  def loopindices_new(flat: Exp[Int], inds: Seq[Exp[Int]]): Exp[LoopIndices] = reflectPure(LoopIndicesNew(flat,inds))

  def indices_new(x: Seq[Exp[Int]]): Exp[Indices] = reflectPure(IndicesNew(x.toList))
  def indices_apply(x: Exp[AbstractIndices], i: Int): Exp[Int] = field[Int](x, "i" + i)
  def loopindices_flat(x: Exp[LoopIndices]): Exp[Int] = field[Int](x, "flat")

  // HACK: Allow index apply of LoopIndices before it's available
  override def field[T:Manifest](struct: Exp[Any], index: String)(implicit pos: SourceContext): Exp[T] = struct match {
    case Def(LoopIndicesNew(f,i)) if index != "flat" && i.isEmpty => reflectPure(FieldApply[T](struct,index))
    case _ => super.field[T](struct,index)
  }

  // HACK: unknown number of fields in struct - can't give full elems list here...
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

trait DeliteAbstractOps extends IndicesOps

/**
 * Base trait for creating nodes with no codegen rules. 
 * Extending trait should look something like:
 *
 * trait FooOpsExp extends DriftOpsExp {
 *   private implicit val fc = AbstractFamily("foo")
 *   case class Foo(...) extends AbstractDef[...]   // can optionally use AbstractDef[...](fc) here, but not needed
 *   ...
 * }
 *
 */
trait DeliteAbstractOpsExp extends DeliteAbstractOps with IndicesOpsExp with AtomicWriteExp { this: DeliteOpsExp =>
  case class AbstractFamily(name: String, var skip: Boolean = false)

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
  abstract class AbstractDefWithManifest2[A:Manifest, B:Manifest, R:Manifest](implicit fc: AbstractFamily) extends AbstractDefWithManifest[A,R] {
    val mB = manifest[B]
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


  abstract class DeliteAbstractLoopNest[A:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteOp[R] with AbstractNode[R] {
    type OpType <: DeliteAbstractLoopNest[A,R]

    // moved to DeliteOp for now
    //def copyTransformedSymListOrElse[B](f: OpType => List[Exp[B]])(e: => List[Exp[B]]): List[Exp[B]] = original.map(p => f(p._2.asInstanceOf[OpType]).map(p._1(_))).getOrElse(e)
    def copyTransformedBlockListOrElse[B:Manifest](f: OpType => List[Block[B]])(e: => List[Block[B]]): List[Block[B]] = original.map(p => f(p._2.asInstanceOf[OpType]).map(p._1(_))).getOrElse(e)
    def copyTransformedOptionOrElse[B:Manifest](f: OpType => Option[Block[B]])(e: => Option[Block[B]]): Option[Block[B]] = original.map(p => f(p._2.asInstanceOf[OpType]).map(p._1(_))).getOrElse(e)

    val nestLevels: Int
    lazy val vs: List[Sym[Int]] = copyTransformedSymListOrElse(_.vs)(List.fill(nestLevels)(fresh[Int])).asInstanceOf[List[Sym[Int]]]
    lazy val is: Sym[Indices] = copyTransformedOrElse(_.is)(indices_new(vs)).asInstanceOf[Sym[Indices]]

    val mA = manifest[A]
    val mR = manifest[R]
    val fm = fc
  }

  abstract class DeliteAbstractLoopNest2[A:Manifest,B:Manifest,R:Manifest](implicit fc: AbstractFamily) extends DeliteAbstractLoopNest[A,R] { val mB = manifest[B] }
}

// TODO: This is approximately what LoweringTransformer was also trying to accomplish
// Can we rectify these two? Not sure how LoweringTransformer should be used exactly
trait DeliteLowerableOpsExp extends DeliteAbstractOpsExp with DeliteVisit { self: DeliteOpsExp => 

  def lower[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = { 
    sys.error("Don't know how to lower " + e)
  }

  abstract class AbstractImplementer(implicit val fc: AbstractFamily) extends TunnelingTransformer {
    val IR: self.type

    // Prior to beginning implementation, set abstract family to "skip" abstract implementation
    // when possible, instead directly generating the implementation. Nodes are not required to 
    // honor this setting, but it can potentially reduce the number of transformation passes if they do.
    override def preprocess[A:Manifest](b: Block[A]): Block[A] = { fc.skip = true; b } 

    // TODO: Change family matching to pointer matching rather than string matching?
    // Potentially requires transformer to be defined in the same trait as the nodes..
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case d: AbstractNode[_] if d.family == fc.name => 
        dbgmsg("Transforming abstract node in family " + d.family)
        Some(lower(d, f)(mtype(s.tp),ctx))
      case d: AbstractNode[_] =>
        dbgmsg("Ignoring abstract node in family " + d.family + " (!= " + fc.name + ")")
        None

      // FIXME: Will this work? Relies on mirroring of lhs of defs right now... 
      /*case NestedAtomicWrite(_,trace,f) =>
        if ((f.isInstanceOf[AbstractNode[_]] && f.asInstanceOf[AbstractNode[_]].family == fc.name) ||
            trace.map{case t: AbstractAtomicTracer => t.family == fc.name; case _ => false }.fold(false){_||_})
          Some(implementNode(s, d)) // This should return a NestedAtomicWrite (assuming it still is one)
        else 
          None  */
      case _ => super.transformSym(s,d)
    }
  }
}
