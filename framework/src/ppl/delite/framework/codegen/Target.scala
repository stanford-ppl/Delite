package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{GenericCodegen, Effects}
import _root_.scala.virtualization.lms.util.GraphUtil
import collection.mutable.{HashMap, ListBuffer}
import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication


/**
 * This trait encodes a target for code generation. The target has a single code generator object containing
 * all of the generator objects for a particular programming model / language.
 */
trait Target {
  val IR: DeliteApplication
  import IR._

  val name: String

  //def generator : GenericCodegen{val IR: Target.this.IR.type}  
}