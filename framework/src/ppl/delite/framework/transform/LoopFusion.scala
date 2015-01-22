package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

/** In order to get loop fusion for your DSL, mix in this trait in your Exp.
  * The DeliteCollections used as loop outputs need to implement the fusion
  * extractors (see for example DeliteArrayExtractors). Also mix
  * scala.virtualization.lms.common.CombineTTPScheduling into your Codegen. */
trait LoopFusionExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with LoopFusionCore with DeliteArrayExtractors { self =>

  override def shouldDoFusion = Config.opfusionEnabled
  
  if (shouldDoFusion) {
    appendTransformer(new LoopFusionVerticalTransformer { val IR: self.type = self })
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
    case f: DeliteForeachElem[_] => f.func match {
      case Block(inner) => Some(inner.asInstanceOf[Exp[Unit]])
    }
    case _ => super.unapplyFor(e)
  }

  // TODO: define this extractor

  // override def unapplyReduce[T](e: Def[T]) = e match {
  //   case MultiReduceElem(Block(valFunc), _, _, _, _) =>
  //     Some(valFunc.asInstanceOf[Exp[T]])
  //   case _ => super.unapplyReduce(e)
  // }
}
