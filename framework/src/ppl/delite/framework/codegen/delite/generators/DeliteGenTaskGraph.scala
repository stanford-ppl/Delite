package ppl.delite.framework.codegen.delite.generators

import java.io.{FileWriter, File, PrintWriter}
import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{ScalaGenEffect, GenericCodegen}
import ppl.delite.framework.{Util, Config}
import collection.mutable.ListBuffer
import java.util.ArrayList

trait DeliteGenTaskGraph extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  private def vals(sym: Sym[_]) : List[Sym[_]] = sym match {
    case Def(Reify(s, effects)) => Nil
    case Def(Reflect(NewVar(v), effects)) => Nil
    case _ => List(sym)
  }

  private def vars(sym: Sym[_]) : List[Sym[_]] = sym match {
    case Def(Reflect(NewVar(v), effects)) => List(sym)
    case _ => Nil
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) : Unit = {
    assert(generators.length >= 1)

    var resultIsVar = false

    // we will try to generate any node that is not purely an effect node
    rhs match {
      case Reflect(s, effects) => super.emitNode(sym, rhs); return
      case Reify(s, effects) => super.emitNode(sym, rhs); return
      case NewVar(x) => resultIsVar = true // if sym is a NewVar, we must mangle the result type
      case _ => // continue and attempt to generate kernel
    }

    // validate that generators agree on inputs (similar to schedule validation in DeliteCodegen)
    val dataDeps = ifGenAgree(g => (g.syms(rhs) ++ g.getFreeVarNode(rhs)).distinct, true)    
    val inVals = dataDeps flatMap { vals(_) }
    val inVars = dataDeps flatMap { vars(_) }
    val metaData = new ArrayList[String]
    
    implicit val supportedTargets = new ListBuffer[String]
    for (gen <- generators) {
      val buildPath = Config.build_dir + gen + "/"
      val outDir = new File(buildPath); outDir.mkdirs()
      val outFile = new File(buildPath + quote(sym) + "." + gen.kernelFileExt)
      val kstream = new PrintWriter(new FileWriter(outFile))

      
      try{
        // emit kernel
        gen.emitKernelHeader(sym, inVals, inVars, resultIsVar)(kstream)
        gen.emitNode(sym, rhs)(kstream)
        gen.emitKernelFooter(sym, inVals, inVars, resultIsVar)(kstream)

        //record that this kernel was succesfully generated
        supportedTargets += gen.toString
        kstream.close()

        //add MetaData
        if(gen.getMetaData!="") metaData.add(gen.getMetaData) 
      }
      catch {
        case e: Exception => // no generator found
          gen.exceptionHandler(outFile, kstream)
      }
    }

    // emit task graph node
    val inputs = inVals ++ inVars
    rhs match {
      case DeliteOP_SingleTask(block) => emitSingleTask(sym, inputs, List(), metaData)
      case DeliteOP_Map(block) => emitMap(sym, inputs, List(), metaData)
      case DeliteOP_ZipWith(block) => emitZipWith(sym, inputs, List(), metaData)
      case _ => emitSingleTask(sym, inputs, List(), metaData) // things that are not specified as DeliteOPs, emit as SingleTask nodes
    }
    
    // whole program gen (for testing)
    //emitValDef(sym, "embedding.scala.gen.kernel_" + quote(sym) + "(" + inputs.map(quote(_)).mkString(",") + ")")
  }

  def emitSingleTask(sym: Sym[_], inputs: List[Exp[_]], antiDeps: List[Sym[_]], metaData:ArrayList[String])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = {
    stream.print("{\"type\":\"SingleTask\"")
    stream.print(",\"kernelId\":\"" + quote(sym) + "\"")
    stream.print(",\"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "]\n")
    val inputsStr = if(inputs.isEmpty) "" else inputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.println("  \"inputs\":[" + inputsStr + "]")
    stream.println("  \"metadata\":"+metaData.toString)
    stream.println("},")
  }
  def emitMap(sym: Sym[_], inputs: List[Exp[_]], antiDeps: List[Sym[_]], metaData:ArrayList[String])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop
  def emitZipWith(sym: Sym[_], inputs: List[Exp[_]], antiDeps: List[Sym[_]], metaData:ArrayList[String])(implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop

  def nop = throw new RuntimeException("Not Implemented Yet")

}
