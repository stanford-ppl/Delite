package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

/** In order to get loop fusion for your DSL, mix in this trait in your Exp.
  * The DeliteCollections used as loop outputs need to implement the fusion
  * extractors (see DeliteArrayExtractors). Also mix
  * scala.virtualization.lms.common.CombineTTPScheduling into your Codegen. */
trait LoopFusionExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with LoopFusionCore with DeliteArrayExtractors { self =>

  override def shouldDoFusion = Config.opfusionEnabled
  
  if (shouldDoFusion) {
    appendTransformer(new LoopFusionVerticalTransformer { val IR: self.type = self })
    appendTransformer(new LoopFusionHorizontalTransformer { val IR: self.type = self })
  }

  // TODO: define these extractors

  // override def unapplyFor(e: Def[Unit]) = e match {
  //   case ForeachElem(Block(a: Exp[Unit @unchecked])) =>
  //     Some(a.asInstanceOf[Exp[Unit]])
  //   case _ => super.unapplyFor(e)
  // }

  // override def unapplyReduce[T](e: Def[T]) = e match {
  //   case MultiReduceElem(Block(valFunc), _, _, _, _) =>
  //     Some(valFunc.asInstanceOf[Exp[T]])
  //   case _ => super.unapplyReduce(e)
  // }

}
