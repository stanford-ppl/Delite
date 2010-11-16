package ppl.delite.framework.codegen.delite.generators

import java.io.{FileWriter, File, PrintWriter}
import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{ScalaGenEffect, GenericCodegen}
import ppl.delite.framework.{Util, Config}
import collection.mutable.ListBuffer

trait DeliteGenTaskGraph extends DeliteCodegen {
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

    

    implicit val supportedTargets = new ListBuffer[String]
    for (gen <- generators) {
      try{
        // emit kernel
        val build_path = Config.build_dir + gen + "/"
        val outf = new File(build_path)
        outf.mkdirs()


        val kstream = new PrintWriter(new FileWriter(build_path + quote(sym) + "." + gen.kernelFileExt))
        kstream.println("package embedding.scala-gen")
        kstream.println("object " + quote(sym) + "{")
        kstream.println("def apply(")
        kstream.println(params.map(p => quote(p) + ":" + p.Type).mkString(","))
        kstream.println(") = {")

        gen.emitNode(sym, rhs)(kstream)

        kstream.println("}}")
        kstream.close()
        //record that this kernel was succesfully generated
        gen.toString match {
          case "C" => supportedTargets += "Native"
          case "CUDA" => supportedTargets += "GPU"
          case "Scala" => supportedTargets += "JVM"          
          case _ => //not supported
        }
      }
      catch {
        case e: Exception => // no generator found
      }
    }

    // emit task graph node
    rhs match {
      case DeliteOP_SingleTask(block) => emitSingleTask(sym, params.toList, List())
      case DeliteOP_Map(block) => emitMap(sym, List(), List())
      case DeliteOP_ZipWith(block) => emitZipWith(sym, List(), List())
      case _ => emitSingleTask(sym, List(), List()) // things that are not specified as DeliteOPs, emit as SingleTask nodes
    }
  }

  def emitSingleTask(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = {
    stream.print("{\"type\":\"SingleTask\"")
    stream.print(",\"kernelId\":\"" + quote(sym) + "\"")
    stream.print(",\"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "]\n")
    val inputsStr = if(inputs.isEmpty) "" else inputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"inputs\":[" + inputsStr + "]")
    stream.println("},")

    //emitValDef(sym, "embedding.scala-gen." + quote(sym) + "(" + (inputs.map(quote(_))).mkString(",") + ")")  
  }
  def emitMap(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop
  def emitZipWith(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop

  def nop = throw new RuntimeException("Not Implemented Yet")

}
