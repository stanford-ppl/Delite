package ppl.dsl.deliszt.vec

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.deliszt.VecView

trait VecViewOps extends Base with OverloadHack {
}

trait VecViewOpsExp extends VecViewOps with BaseExp { this: DeliteOpsExp =>
}

trait ScalaGenVecViewOps extends ScalaGenBase {
  val IR: VecViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}