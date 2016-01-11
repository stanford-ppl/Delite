package ppl.delite.framework.ops

import scala.reflect.SourceContext
import scala.virtualization.lms.common.AtomicWrites
import scala.virtualization.lms.internal.FatExpressions

import ppl.delite.framework.transform.TunnelingTransformer
import ppl.delite.framework.datastructures._

trait DeliteFigmentsExp extends AtomicWrites with FatExpressions { self =>

  trait FigNode[A] extends Def[A]

  abstract class Fig[R:Manifest] extends FigNode[R] {
    val mR = manifest[R]
  }
  abstract class Fig2[A:Manifest, R:Manifest] extends Fig[R] { val mA = manifest[A] }
  abstract class Fig3[A:Manifest, B:Manifest, R:Manifest] extends Fig2[A,R] { val mB = manifest[B] }

  abstract class AtomicWriteFig[A:Manifest] extends FigNode[Unit] with AtomicWrite[A] { val mA = manifest[A] }
  abstract class AtomicTracerFig extends AtomicTracer

  // TODO: How to quickly check for convergence here?
  abstract class Implementer extends TunnelingTransformer
}

trait DeliteFigmentOpsExp extends DeliteFigmentsExp with AbstractIndicesOpsExp { this: DeliteOpsExp =>

  abstract class OpFig[R:Manifest] extends DeliteOp[R] with FigNode[R] {
    type OpType <: OpFig[R]
    val mR = manifest[R]
  }

  // TODO: Should one inherit from the other? Both are types of loops...
  abstract class OpLoopFig[R:Manifest] extends OpFig[R] {
    type OpType <: OpLoopFig[R]
    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
  }
  abstract class OpLoopFig2[A:Manifest,R:Manifest] extends OpLoopFig[R] { val mA = manifest[A] }
  abstract class OpLoopFig3[A:Manifest,B:Manifest,R:Manifest] extends OpLoopFig2[A,R] { val mB = manifest[B] }
  abstract class OpLoopFig4[A:Manifest,B:Manifest,C:Manifest,R:Manifest] extends OpLoopFig3[A,B,R] { val mC = manifest[C] }

  abstract class NestedLoopFig[R:Manifest] extends OpFig[R] {
    type OpType <: NestedLoopFig[R]
    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
    val i: Sym[LoopIndices] = copyOrElse(_.i)(loopIndicesEmpty(v).asInstanceOf[Sym[LoopIndices]])
  }
  abstract class NestedLoopFig2[A:Manifest,R:Manifest] extends NestedLoopFig[R] { val mA = manifest[A] }
  abstract class NestedLoopFig3[A:Manifest,B:Manifest,R:Manifest] extends NestedLoopFig2[A,R] { val mB = manifest[B] }
  abstract class NestedLoopFig4[A:Manifest,B:Manifest,C:Manifest,R:Manifest] extends NestedLoopFig3[A,B,R] { val mC = manifest[C] }
}
