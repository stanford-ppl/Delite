package ppl.delite.framework.datastructures

import scala.virtualization.lms.common.{ScalaGenBase, BaseExp}
import java.io.PrintWriter

trait FieldAccessOpsExp extends BaseExp {

  case class FieldRead[T](o: Exp[_], f: String, t: String) extends Def[T]

}

trait ScalaGenFieldAccessOps extends ScalaGenBase {
  val IR: FieldAccessOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case FieldRead(o,f,_) => emitValDef(sym, quote(o) + "." + f)
    case _ => super.emitNode(sym, rhs)
  }
}