package ppl.delite.framework.codegen.c

import java.io.PrintWriter
import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.Target
import scala.virtualization.lms.common.embedded.scala.CGenMiscOps

trait TargetC extends Target {
  import intermediate._

  val name = "C"

  lazy val generator = new CCodeGenPkg { val IR: TargetC.this.intermediate.type = TargetC.this.intermediate }
}

// TODO: move to Packages.scala?
trait CCodeGenPkg extends CGenMiscOps