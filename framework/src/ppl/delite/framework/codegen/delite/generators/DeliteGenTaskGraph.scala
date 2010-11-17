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

  private def unwrapSym(sym: Sym[_]) : Option[Exp[_]] =
    findDefinition(sym).flatMap {
      _.rhs match {
        //case Reflect(s, effects) => Some(toAtom(s))
        case Reify(s, effects) => None
        case _ => Some(sym)
      }
    }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) : Unit = {
    assert(generators.length >= 1)

    // we will try to generate any node that is not purely an effect node
    rhs match {
      case Reflect(s, effects) => super.emitNode(sym, rhs); return
      case Reify(s, effects) => super.emitNode(sym, rhs); return
      case _ => // continue and attempt to generate kernel
    }

    val params = scala.collection.mutable.Queue[Exp[Any]]()
    // TODO: validate that generators agree on inputs (similar to schedule validation in DeliteCodegen)
    val data_deps = (generators(0).inputs(rhs) ++ generators(0).getFreeVarNode(rhs)).distinct
    for (input <- data_deps) {
      input match {
        case c: Const[Any] => // don't need to pass in
        case d: Def[Any] => params += toAtom(d)
        case Variable(e) => params += e
        case s: Sym[Any] => val o = unwrapSym(s); if (!o.isEmpty) params += o.get
        case e: Exp[Any] => params += e
        case _ => println("encountered unexpected input symbol in kernel generation: "
                          + input + " for symbol: " + quote(sym) )
      }
    }
    //val params = data_deps.map(param(_)).flatMap(_.getOrElse(Nil))

    implicit val supportedTargets = new ListBuffer[String]
    for (gen <- generators) {
      val build_path = Config.build_dir + gen + "/"
      val outf = new File(build_path)
      outf.mkdirs()
      val kstream = new PrintWriter(new FileWriter(build_path + quote(sym) + "." + gen.kernelFileExt))

      try{
        // emit kernel
        kstream.println("package embedding." + gen + ".gen")
        kstream.println("object kernel_" + quote(sym) + "{")
        kstream.println("def apply(")
        kstream.println(params.map(p => quote(p) + ":" + gen.remap(p.Type)).mkString(","))
        kstream.println(") = {")

        gen.emitNode(sym, rhs)(kstream)

        kstream.println("}}")

        //record that this kernel was succesfully generated
        gen.toString match {
          case "c" => supportedTargets += "Native"
          case "CUDA" => supportedTargets += "GPU"
          case "scala" => supportedTargets += "JVM"          
          case _ => //not supported
        }
      }
      catch {
        case e: Exception => // no generator found
      }
      finally {
        kstream.close()
      }
    }

    // emit task graph node
    rhs match {
      case DeliteOP_SingleTask(block) => emitSingleTask(sym, params.toList, List())
      case DeliteOP_Map(block) => emitMap(sym, List(), List())
      case DeliteOP_ZipWith(block) => emitZipWith(sym, List(), List())
      case _ => emitSingleTask(sym, List(), List()) // things that are not specified as DeliteOPs, emit as SingleTask nodes
    }


    // whole program gen (for testing)
    //emitValDef(sym, "embedding.scala.gen.kernel_" + quote(sym) + "(" + (params.map(quote(_))).mkString(",") + ")")
  }

  def emitSingleTask(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = {
    stream.print("{\"type\":\"SingleTask\"")
    stream.print(",\"kernelId\":\"" + quote(sym) + "\"")
    stream.print(",\"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "]\n")
    val inputsStr = if(inputs.isEmpty) "" else inputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"inputs\":[" + inputsStr + "]")
    stream.println("},")
  }
  def emitMap(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop
  def emitZipWith(sym: Sym[_], inputs: List[Exp[_]], control_deps: List[Sym[_]])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop

  def nop = throw new RuntimeException("Not Implemented Yet")

}
