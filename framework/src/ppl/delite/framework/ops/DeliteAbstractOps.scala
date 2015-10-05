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
