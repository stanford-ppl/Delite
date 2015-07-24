package ppl.delite.framework.codegen.hw

import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
//import ppl.delite.framework.codegen.hw.HwCodegen
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

// Analysis passes
// import ppl.delite.framework.analysis.MetaPipelineAnalysis
import ppl.delite.framework.analysis.DotPrintAnalysis

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack


trait HwDummy extends HwCodegen {
  val IR: DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    super.emitNode(sym, rhs)
  }
}
