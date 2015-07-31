package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import java.io.PrintWriter

/*
 * @PrintAnalysis: Prints a block
 */
trait PrintAnalysis extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._

  private var outFileStream: PrintWriter = null

  def run[A](body: Block[Any], name: String = "", pw: PrintWriter = null) = {
    outFileStream = pw
    println(s"[PrintAnalysis - Begin][$name]")
    traverseBlock(body)
    println(s"[PrintAnalysis - End][$name]")
  }

  override def traverseStm(stm: Stm): Unit = {
    if (outFileStream != null) {
      outFileStream.println(stm)
    } else {
      println(stm)
    }
  }
}
