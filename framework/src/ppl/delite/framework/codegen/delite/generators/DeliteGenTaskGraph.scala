package ppl.delite.framework.codegen.delite.generators

import java.io.{FileWriter, File, PrintWriter}
import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import scala.virtualization.lms.internal.{ScalaGenEffect, GenericCodegen}

trait DeliteGenTaskGraph extends ScalaGenEffect {
  val IR: DeliteOpsExp
  import IR._

  val generators : List[GenericCodegen{val IR: DeliteGenTaskGraph.this.IR.type}]

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {

    val params = scala.collection.mutable.Queue[Exp[Any]]()
    for (input <- inputs(rhs)) {
      input match {
        case c: Const[Any] => // don't need to pass in
        case e: Exp[Any] => params += e
        case d: Def[Any] => params += toAtom(d)
        case Variable(e) => params += e
        case _ => println("encountered unexpected input symbol in kernel generation: " + input + " for symbol: " + quote(sym) )
      }
    }

    // TODO: record which generators succeeded
    for (gen <- generators) {
      try{
        // emit kernel
        val build_path = Config.build_dir + gen + "/"
        val outf = new File(build_path)
        outf.mkdirs()

        val kstream = new PrintWriter(new FileWriter(build_path + quote(sym)))
        kstream.println("package embedding.scala-gen")
        kstream.println("object " + quote(sym) + "{")
        kstream.println("def apply(")
        kstream.println(params.map(p => quote(p) + ":" + p.Type).mkString(","))
        kstream.println(") = {")

        gen.emitNode(sym, rhs)(kstream)

        kstream.println("}}")
        kstream.close()
      }
      catch {
        case e: Exception => // no generator found
      }
    }

    // emit task graph node
    rhs match {
      case DeliteOP_SingleTask(block) => emitSingleTask(sym, List(), List())
      case DeliteOP_Map(block) => emitMap(sym, List(), List())
      case DeliteOP_ZipWith(block) => emitZipWith(sym, List(), List())
      case _ => emitSingleTask(sym, List(), List()) // things that are not specified as DeliteOPs, emit as SingleTask nodes
    }

    // whole program gen
    emitValDef(sym, "embedding.scala-gen." + quote(sym) + "(" + (params.map(quote(_))).mkString(",") + ")")
  }

  def emitSingleTask(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter) {}
  def emitMap(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter) {}
  def emitZipWith(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter) {}

}
