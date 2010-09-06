package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{GenericNestedCodegen}
import java.io.PrintWriter

trait CodeGenerator {

  val intermediate: BaseExp
  import intermediate._

  def emitSource[A,B](app:GenericNestedCodegen, f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit
  
}