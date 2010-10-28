package ppl.delite.framework.codegen.scala

import java.io.PrintWriter
import collection.mutable.ListBuffer
import scala.virtualization.lms.common.embedded.scala.ScalaCodeGenPkg
import ppl.delite.framework.codegen.Target
import scala.virtualization.lms.internal.{ScalaCodegen}

trait TargetScala extends Target {
  import IR._

  val name = "Scala"

  //lazy val generator = new ScalaCodeGenPkg with DSLCodeGenPkg { val IR: TargetScala.this.IR.type = TargetScala.this.IR }
}