package ppl.delite.framework.ops

import _root_.scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.internal.{GenericCodegen, ScalaGenBase}
import java.io.{FileWriter, File, PrintWriter}

trait DeliteOpsExp extends EffectExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).

  case class DeliteOP_SingleTask[A](val block: Exp[A]) extends Def[A]
  case class DeliteOP_Map[A](val func: Exp[A]) extends Def[A]
  case class DeliteOP_ZipWith[A](val func: Exp[A]) extends Def[A]
}