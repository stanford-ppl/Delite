package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.Effects
import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication


trait CodeGeneratorApplication extends CodeGenerator {
  import intermediate._

  def emitSource[A,B](x: Exp[A], f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B] ): Unit
}