package ppl.delite.framework.ops

import scala.reflect.SourceContext
import scala.virtualization.lms.common.AtomicWrites
import scala.virtualization.lms.internal.FatExpressions

import ppl.delite.framework.transform.TunnelingTransformer
import ppl.delite.framework.datastructures._

trait DeliteAbstractDefsExp extends AtomicWrites with FatExpressions { self =>
  case class AbstractFamily(name: String, var skip: Boolean = false)

  trait AbstractNode[A] extends Def[A] {
    val fm: AbstractFamily
    def family = fm.name
  }

  abstract class AbstractDef[R:Manifest](implicit fc: AbstractFamily) extends AbstractNode[R] {
    val fm = fc
    val mR = manifest[R]
  }
  abstract class AbstractDef2[A:Manifest, R:Manifest](implicit fc: AbstractFamily) extends AbstractDef[R] {
    val mA = manifest[A]
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

  abstract class AbstractOp[R:Manifest](implicit fc: AbstractFamily) extends DeliteOp[R] with AbstractNode[R] {
    type OpType <: AbstractOp[R]
    val mR = manifest[R]
    val fm = fc
  }

  // TODO: Should one inherit from the other? Both are types of loops...
  abstract class AbstractOpLoop[R:Manifest](implicit fc: AbstractFamily) extends AbstractOp[R] {
    type OpType <: AbstractOpLoop[R]
    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
  }
  abstract class AbstractOpLoop2[A:Manifest,R:Manifest](implicit fc: AbstractFamily) extends AbstractOpLoop[R] {
    val mA = manifest[A]
  }
  abstract class AbstractOpLoop3[A:Manifest,B:Manifest,R:Manifest](implicit fc: AbstractFamily) extends AbstractOpLoop2[A,R] {
    val mB = manifest[B]
  }
  abstract class AbstractOpLoop4[A:Manifest,B:Manifest,C:Manifest,R:Manifest](implicit fc: AbstractFamily) extends AbstractOpLoop3[A,B,R] {
    val mC = manifest[C]
  }

  abstract class AbstractNestedLoop[R:Manifest](implicit fc: AbstractFamily) extends AbstractOp[R] {
    type OpType <: AbstractNestedLoop[R]
    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
    val i: Sym[LoopIndices] = copyOrElse(_.i)(loopIndicesEmpty(v).asInstanceOf[Sym[LoopIndices]])
  }
  abstract class AbstractNestedLoop2[A:Manifest,R:Manifest](implicit fc: AbstractFamily) extends AbstractNestedLoop[R] {
    val mA = manifest[A]
  }
  abstract class AbstractNestedLoop3[A:Manifest,B:Manifest,R:Manifest](implicit fc: AbstractFamily) extends AbstractNestedLoop2[A,R] {
    val mB = manifest[B]
  }
  abstract class AbstractNestedLoop4[A:Manifest,B:Manifest,C:Manifest,R:Manifest](implicit fc: AbstractFamily) extends AbstractNestedLoop3[A,B,R] {
    val mC = manifest[C]
  }
}
