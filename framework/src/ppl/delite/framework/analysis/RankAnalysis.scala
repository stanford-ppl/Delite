package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteMultiArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

trait RankAnalysis extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._

  var multiaRanks = new HashMap[Exp[DeliteMultiArray[Any]],MultiArrayMetadata]


}