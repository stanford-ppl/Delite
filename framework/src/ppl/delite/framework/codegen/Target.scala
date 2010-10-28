package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{GenericCodegen, Effects}
import _root_.scala.virtualization.lms.util.GraphUtil
import collection.mutable.{HashMap, ListBuffer}
import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication


/**
 * This trait encodes a target for code generation, the target can have multiple code generators registered
 */
trait Target {

  val intermediate: DeliteApplication
  import intermediate._

  val name: String

  def generator : GenericCodegen{val IR: Target.this.intermediate.type}
}