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

  private def mutating(kernelContext: State, sym: Sym[_]) : List[Sym[_]] =
    kernelContext flatMap {
      //case Def(Reflect(x,effects)) => if (syms(x) contains sym) List(sym) else Nil
      case Def(Mutation(x,effects)) => if (syms(x) contains sym) List(sym) else Nil
      case _ => Nil: List[Sym[_]]
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

    val inputs = inVals ++ inVars
    //val kernelContext = getEffectsKernel(sym, rhs)
    val kernelContext = ifGenAgree( _.getEffectsBlock(sym), true )
    val inMutating = (inputs flatMap { mutating(kernelContext, _) }).distinct

    // additional data deps: for each of my inputs, look at the kernels already generated and see if any of them
    // mutate it, and if so, add that kernel as a data-dep
    val extraDataDeps = (kernelMutatingDeps filter { case (s, mutates) => (!(inputs intersect mutates).isEmpty) }).keys
    val inControlDeps = (controlDeps ++ extraDataDeps).distinct

    // anti deps: for each of my mutating inputs, look at the kernels already generated and see if any of them
    // read it, add that kernel as an anti-dep
    val antiDeps = (kernelInputDeps filter { case (s, in) => (!(inMutating intersect in).isEmpty) }).keys.toList

    // add this kernel to global generated state
    kernelInputDeps += { sym -> inputs }
    kernelMutatingDeps += { sym -> inMutating }

    // debug
    /*
    stream.println("inputs: " + inputs)
    stream.println("mutating inputs: " + inMutating)
    stream.println("extra data deps: " + extraDataDeps)
    stream.println("control deps: " + inControlDeps)
    stream.println("anti deps:" + antiDeps)
    */
    
    // emit task graph node
    rhs match {
      case DeliteOP_SingleTask(block) => emitSingleTask(sym, inputs, inControlDeps, antiDeps, metaData)
      case DeliteOP_Map(block) => emitMap(sym, inputs, inControlDeps, antiDeps, metaData)
      case DeliteOP_ZipWith(block) => emitZipWith(sym, inputs, inControlDeps, antiDeps, metaData)
      case _ => emitSingleTask(sym, inputs, inControlDeps, antiDeps, metaData) // things that are not specified as DeliteOPs, emit as SingleTask nodes
    }

    // whole program gen (for testing)
    //emitValDef(sym, "embedding.scala.gen.kernel_" + quote(sym) + "(" + inputs.map(quote(_)).mkString(",") + ")")
  }

  /**
   * @param sym         the symbol representing the kernel
   * @param inputs      a list of real kernel dependencies (formal kernel parameters)
   * @param controlDeps a list of control dependencies (must execute before this kernel)
   * @param antiDeps    a list of WAR dependencies (need to be committed in program order)
   * @param metaData    a list of metaData strings
   */
  def emitSingleTask(sym: Sym[_], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]], metaData:ArrayList[String])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = {
    stream.print("{\"type\":\"SingleTask\"")
    stream.print(",\"kernelId\":\"" + quote(sym) + "\"")
    stream.print(",\"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "]\n")
    val inputsStr = if(inputs.isEmpty) "" else inputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"inputs\":[" + inputsStr + "]\n")
    val controlDepsStr = if(controlDeps.isEmpty) "" else controlDeps.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"controlDeps\":[" + controlDepsStr + "]\n")
    val antiDepsStr = if(antiDeps.isEmpty) "" else antiDeps.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"antiDeps\":[" + antiDepsStr + "]\n")
    stream.println("  \"metadata\":"+metaData.toString)
    stream.println("},")
  }
  
  def emitMap(sym: Sym[_], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]], metaData:ArrayList[String])
             (implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop
  def emitZipWith(sym: Sym[_], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]], metaData:ArrayList[String])
                 (implicit stream: PrintWriter, supportedTgt: ListBuffer[String]) = nop

  def nop = throw new RuntimeException("Not Implemented Yet")

}
