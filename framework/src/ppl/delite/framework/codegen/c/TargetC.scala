package ppl.delite.framework.codegen.c

import java.io.PrintWriter
import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.Target
import scala.virtualization.lms.common.embedded.scala.{CCodeGenPkg, CGenMiscOps}

trait TargetC extends Target {
  import IR._

  val name = "C"

  //lazy val generator = new CCodeGenPkg with DSLCodeGenPkg { val IR: TargetC.this.IR.type = TargetC.this.IR }
}