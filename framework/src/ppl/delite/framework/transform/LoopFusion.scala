package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

/** In order to get loop fusion for your DSL, mix in this trait in your Exp.
  * The DeliteCollections used as loop outputs need to implement the fusion
  * extractors in LoopFusionCore (see for example DeliteArrayExtractors). Also
  * mix scala.virtualization.lms.common.CombineTTPScheduling into your
  * Codegen. */
trait LoopFusionExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with LoopFusionCore with DeliteArrayExtractors { self =>

  override def shouldDoFusion = Config.opfusionEnabled
  
  if (shouldDoFusion) {
    appendTransformer(new LoopFusionVerticalTransformer { val IR: self.type = self })
    // If you disable only horizontal fusion, some apps might fail to compile
    // (loop index var redefinition) because vertical fusion mirrors
    // neighboring loops to use the same index, and horizontal fusion
    // re-uniquifies the indices.
    // Possible fixes: use for-loops, emit scopes around inline loops (doesn't
    // work for scala), create LMS transformer that only does index
    // reunification of non-fused loops.
    appendTransformer(new LoopFusionHorizontalTransformer { val IR: self.type = self })
  }

  // General fusion extractors that aren't specific to the collection type

  override def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = e match {
    case EatReflect(loop: AbstractLoop[_]) => loop.body match {
      case elem: DeliteCollectElem[_,_,_] if (elem.unknownOutputSize) => None
      case _ => super.unapplyFixedDomain(e)
    }
    case _ => super.unapplyFixedDomain(e)
  }

  override def unapplyMultiCollect[T](e: Def[T]) = e match {
    case c: DeliteCollectElem[_,_,_] => c.iFunc match {
      case Block(inner) => Some(inner.asInstanceOf[Exp[T]])
    }
    case _ => super.unapplyMultiCollect(e)
  }

  override def unapplyFor(e: Def[Unit]) = e match {
    // Note: RangeForeach is not fused because it represents a sequential
    // foreach, and fusing a parallel collect with a RangeForeach would
    // result in a fused operation that is sequential, not parallel.
    case f: DeliteForeachElem[_] => f.func match {
      case Block(inner) => Some(inner.asInstanceOf[Exp[Unit]])
    }
    case _ => super.unapplyFor(e)
  }

  override def unapplyReduce[T](e: Def[T]) = e match {
    case r: DeliteReduceElem[_] => r.iFunc match {
      case Block(inner) => Some(inner.asInstanceOf[Exp[T]])
    }
    case f: DeliteFoldElem[_,_] => f.iFunc match {
      case Block(inner) => Some(inner.asInstanceOf[Exp[T]])
    }
    case _ => super.unapplyReduce(e)
  }
}
