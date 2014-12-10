package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait LoopFusionExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with LoopFusionCore { self =>

  // val IR: DeliteOpsExp
  // import IR._

  override def shouldDoFusion = Config.opfusionEnabled
  
  if (shouldDoFusion) {
    appendTransformer(new LoopFusionVerticalTransformer { val IR: self.type = self })
    appendTransformer(new LoopFusionHorizontalTransformer { val IR: self.type = self })
  }

  // def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  // def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = None
  // def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = None

  // def unapplyEmptyColl(a: Def[Any]): Boolean = false
  // def unapplyEmptyCollNewEmpty[T:Manifest](a: (Def[Any], Exp[T], Option[Sym[Int]])): Option[Exp[T]] = None

  // def unapplySingletonColl(a: Def[Any]): Option[Exp[Any]] = None
  // def unapplyMultiCollect[T](a: Def[T]): Option[Exp[T]] = None

  // // exp is valueFunc block of reduce and bodies of for and foreach
  // // boolean is true if type is Unit (for and foreach), false otherwise (reduce usually)
  // def unapplyForlike[T](e: Def[T]): Option[(Exp[T], Boolean)] = None

  // def ignoreIndex(e: Def[Any], index: Sym[Int]): Boolean = false




  /*
    // TODO: can implement generically? or need override for VectorSize and all others?
    override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
      case ArrayLength(a) => Some(a)
      case _ => super.unapplySimpleDomain(e)
    }
  */

  // override def unapplySimpleCollect(e: Def[Any]) = e match {
  //   case e: DeliteCollectElem[_,_,_] if e.cond.isEmpty => Some(e.func.res)
  //   case _ => super.unapplySimpleCollect(e)
  // }

  // override def unapplySimpleCollectIf(e: Def[Any]) = e match {
  // //    case e: DeliteReduceElem[_] => Some((e.func, e.cond)) // TODO: aks -- testing fusing conditionals for reduce elems
  //   case e: DeliteHashReduceElem[_,_,_,_] => Some((e.valFunc.res, e.cond.map(_.res))) // FIXME: HACK!!
  //   case e: DeliteCollectElem[_,_,_] => Some((e.func.res, e.cond.map(_.res)))
  //   case _ => super.unapplySimpleCollectIf(e)
  // }

  // // FIXME: need to modify .par from ParPlat to ParBuf accordingly
  // override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match {
  //   case e: DeliteHashCollectElem[_,_,_,_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mV,e.mI,e.mCV,e.mCI,e.mCCV)
  //   case e: DeliteHashReduceElem[_,_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mV,e.mI,e.mCV)
  //   case e: DeliteHashIndexElem[_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mCV)
  //   case e: DeliteCollectElem[_,_,_] => e.copy(par = ParBuffer, cond = e.cond ++ c.map(Block(_)))(e.mA, e.mI, e.mCA)
  //   case e: DeliteReduceElem[_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mA)
  //   case e: DeliteReduceTupleElem[_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mA,e.mB)
  //   case _ => super.applyAddCondition(e,c)
  // }
}
