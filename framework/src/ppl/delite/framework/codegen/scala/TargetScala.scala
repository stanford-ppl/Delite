package ppl.delite.framework.codegen.scala

import java.io.PrintWriter
import collection.mutable.ListBuffer
import scala.virtualization.lms.common.embedded.scala.ScalaCodeGenPkg
import ppl.delite.framework.codegen.Target


trait TargetScala extends Target {

  import intermediate._

  val name = "Scala"

  lazy val generator = new ScalaCodeGenPkg { val IR: TargetScala.this.intermediate.type = TargetScala.this.intermediate }  
}