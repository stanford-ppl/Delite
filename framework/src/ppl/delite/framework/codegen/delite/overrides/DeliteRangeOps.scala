package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.RangeOpsExp
import ppl.delite.framework.ops.DeliteOpsExp

trait DeliteRangeOpsExp extends RangeOpsExp {
  this: DeliteOpsExp =>

  case class DeliteRangeForEach(start: Exp[Int], end: Exp[Int], i: Exp[Int], body: Exp[Unit])
    extends DeliteOpIndexedLoop(start, end, i, body)

  override def range_foreach(r: Exp[Range], block: Exp[Int] => Exp[Unit]) : Exp[Unit] = {
    val i = fresh[Int]
    //reflectEffect(RangeForeach(r, i, reifyEffects(block(i))))
    val (start,end) = r match {
      case Def(Until(start,end)) => (start,end)
      case _ => throw new Exception("unexpected symbol in RangeForeach")
    }
    reflectEffect(DeliteRangeForEach(start, end, i, reifyEffects(block(i))))
  }
}