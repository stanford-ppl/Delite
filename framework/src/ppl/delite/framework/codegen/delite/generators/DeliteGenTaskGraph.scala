package ppl.delite.framework.codegen.delite.generators

import scala.virtualization.lms.internal.{GenericCodegen}
import java.io.{FileWriter, File, PrintWriter}
import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.DeliteOpsExp

trait DeliteGenTaskGraph extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  val generators : List[GenericCodegen{val IR: DeliteGenTaskGraph.this.IR.type}]

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {

    // emit kernel
    val build_path = /*Config.build_dir +*/ "scala/"
    val outf = new File(build_path)
    outf.mkdirs()

    val kstream = new PrintWriter(new FileWriter(build_path + quote(sym)))
    kstream.println("package embedding.scala-gen")
    kstream.println("object " + quote(sym) + "{")
    kstream.println("def apply() = {")

    // TODO: record which generators succeeded
    for (gen <- generators) {
      try{
        gen.emitNode(sym, rhs)(kstream)
      }
      catch {
        case e: Exception => // no generator found
      }
    }

    kstream.println("}}")
    kstream.close()

    // emit task graph node
    rhs match {
      case DeliteOP_SingleTask(block) => emitSingleTask(sym, List(), List())
      case DeliteOP_Map(block) => emitMap(sym, List(), List())
      case DeliteOP_ZipWith(block) => emitZipWith(sym, List(), List())
      case _ => emitSingleTask(sym, List(), List()) // things that are not specified as DeliteOPs, emit as SingleTask nodes
    }
  }

  def emitSingleTask(sym: Sym[_], data_deps: List[Sym[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter) {}
  def emitMap(sym: Sym[_], data_deps: List[Sym[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter) {}
  def emitZipWith(sym: Sym[_], data_deps: List[Sym[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter) {}

}

