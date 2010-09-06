package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen}
import java.io.PrintWriter

abstract class CodeGenerator {

  def emitSource[A,B, Exp[T]](app:GenericNestedCodegen, f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit
  
}