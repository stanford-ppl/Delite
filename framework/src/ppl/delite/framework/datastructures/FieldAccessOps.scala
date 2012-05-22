package ppl.delite.framework.datastructures

import scala.virtualization.lms.common.{ScalaGenFat, BaseFatExp}
import scala.virtualization.lms.internal.{Effects}
import java.io.PrintWriter
import scala.reflect.SourceContext

trait FieldAccessOpsExp extends BaseFatExp {

  case class FieldRead[T](o: Exp[_], f: String, t: String) extends Def[T]
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = e match {
    case FieldRead(o,fld,t) => FieldRead[A](f(o), fld , t)
    case _ => super.mirror(e,f)
  }

}

trait ScalaGenFieldAccessOps extends ScalaGenFat {
  val IR: FieldAccessOpsExp with Effects
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FieldRead(o,f,_) => emitValDef(sym, quote(o) + "." + f)
    case _ => super.emitNode(sym, rhs)
  }
}