package ppl.delite.framework.codegen.delite.generators

import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.{DeliteOpsExp}
import scala.virtualization.lms.internal.GenericCodegen
import scala.virtualization.lms.common.LoopFusionOpt
import ppl.delite.framework.{Util, Config}
import collection.mutable.{ArrayBuffer, ListBuffer, HashMap}
import java.io.{StringWriter, FileWriter, File, PrintWriter}

trait DeliteGenTaskGraph extends DeliteCodegen with LoopFusionOpt {
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


  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match { // getFreeVarBlock(syms(rhs), boundSyms(rhs))
    case Reflect(s, effects) => getFreeVarNode(s)
    case _ => super.getFreeVarNode(rhs)
  }


  override def emitFatNode(sym: List[Sym[_]], rhs: FatDef)(implicit stream: PrintWriter): Unit = {
  //override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) : Unit = {
    assert(generators.length >= 1)

    println("DeliteGenTaskGraph.emitNode "+sym+"="+rhs)

    val kernelName = sym.map(quote).mkString("")

    var resultIsVar = false

    // we will try to generate any node that is not purely an effect node
    rhs match {
      case ThinDef(Reflect(s, effects)) => super.emitFatNode(sym, rhs); return
      case ThinDef(Reify(s, effects)) => super.emitFatNode(sym, rhs); return
      case ThinDef(NewVar(x)) => resultIsVar = true // if sym is a NewVar, we must mangle the result type
      case _ => // continue and attempt to generate kernel
    }

    //TR: syms might contains stuff that is actually not free (i.e. defined within the node)
    // validate that generators agree on inputs (similar to schedule validation in DeliteCodegen)
    
    val dataDeps = //TR TODO: focus only once!
      syms(rhs).flatMap(s => focusBlock(s) { freeInScope(boundSyms(rhs), s) } ).distinct // don't use getFreeVarNode...
    
    //val dataDeps = /*syms(rhs) ++ */getFreeVarNode(rhs).distinct //ifGenAgree(g => (g.syms(rhs) ++ g.getFreeVarNode(rhs)).distinct, true)
    val inVals = dataDeps flatMap { vals(_) }
    val inVars = dataDeps flatMap { vars(_) }

    implicit val supportedTargets = new ListBuffer[String]
    implicit val returnTypes = new ListBuffer[Pair[String, String]]
    implicit val outputSlotTypes = new HashMap[String, ListBuffer[(String, String)]]
    implicit val metadata = new ArrayBuffer[Pair[String, String]]

    for (gen <- generators) {
      val buildPath = Config.build_dir + gen + "/"
      val outDir = new File(buildPath); outDir.mkdirs()
      val outFile = new File(buildPath + kernelName + "." + gen.kernelFileExt)
      val kstream = new PrintWriter(outFile)
      val bodyString = new StringWriter()
      val bodyStream = new PrintWriter(bodyString)

      try{
        // DISCUSS: use a predicate insteaf of inheriting from DeliteOp?
        rhs match {
//          case op:DeliteFatOp => deliteKernel = true
          case op:AbstractFatLoop => deliteKernel = true
          case ThinDef(op:DeliteOp[_]) => deliteKernel = true
          case _ => deliteKernel = false
        }

        //initialize
        gen.kernelInit(sym, inVals, inVars, resultIsVar)

        // emit kernel to bodyStream //TODO: must kernel body be emitted before kernel header?
        gen.emitFatNode(sym, rhs)(bodyStream)
        bodyStream.flush

        var hasOutputSlotTypes = false
        
        val resultType: String = (gen.toString, rhs) match {
          case ("scala", op: AbstractFatLoop) =>
            hasOutputSlotTypes = true
            "generated.scala.DeliteOpMultiLoop[" + "activation_"+kernelName + "]"
          case ("scala", ThinDef(z)) => z match {
            case op: AbstractLoop[_] => system.error("should not encounter thin loops here but only fat ones")
            case map: DeliteOpMap[_,_,_] => "generated.scala.DeliteOpMap[" + gen.remap(map.v.Type) + "," + gen.remap(map.func.Type) + "," + gen.remap(map.alloc.Type) + "]"
            case zip: DeliteOpZipWith[_,_,_,_] => "generated.scala.DeliteOpZipWith[" + gen.remap(zip.v._1.Type) + "," + gen.remap(zip.v._2.Type) + "," + gen.remap(zip.func.Type) + "," + gen.remap(zip.alloc.Type) +"]"
            case red: DeliteOpReduce[_] => "generated.scala.DeliteOpReduce[" + gen.remap(red.func.Type) + "]"
            case mapR: DeliteOpMapReduce[_,_,_] => "generated.scala.DeliteOpMapReduce[" + gen.remap(mapR.mV.Type) + "," + gen.remap(mapR.reduce.Type) + "]"
            case foreach: DeliteOpForeach[_,_] => "generated.scala.DeliteOpForeach[" + gen.remap(foreach.v.Type) + "]"
            case _ => gen.remap(sym.head.Type)
          }
          case _ => 
            assert(sym.length == 1) // if not set hasOutputSlotTypes and use activation record
            gen.remap(sym.head.Type)
        }

        assert(hasOutputSlotTypes || sym.length == 1)

        // emit kernel
        gen.emitKernelHeader(sym, inVals, inVars, resultType, resultIsVar)(kstream)
        kstream.println(bodyString.toString)
        gen.emitKernelFooter(sym, inVals, inVars, resultType, resultIsVar)(kstream)

        //record that this kernel was successfully generated
        supportedTargets += gen.toString
        if (!hasOutputSlotTypes) { // return type is sym type
          if (resultIsVar) {
            returnTypes += new Pair[String,String](gen.toString,"generated.scala.Ref[" + gen.remap(sym.head.Type) + "]") {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
            }
          } else {
            returnTypes += new Pair[String,String](gen.toString,gen.remap(sym.head.Type)) {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
            }
          }
        } else { // return type is activation record
          returnTypes += new Pair[String,String](gen.toString,"activation_" + kernelName) {
            override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
          }
          for (s <- sym) {
            outputSlotTypes.getOrElseUpdate(quote(s), new ListBuffer) += new Pair[String,String](gen.toString,gen.remap(s.Type)) {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
            }
          }
        }

        //add MetaData
        if(gen.hasMetaData) {
          metadata += new Pair[String,String](gen.toString, gen.getMetaData) {
            override def toString = "\"" + _1 + "\" : " + _2
          }
        }

        kstream.close()
      }
      catch {
        case e: Exception => // no generator found
          if (gen.toString == "scala") e.printStackTrace
          gen.exceptionHandler(outFile, kstream)
      }
    }

    if (supportedTargets.isEmpty) system.error("Node " + quote(sym) + " could not be generated by any code generator")

    val outputs = sym
    
    val inputs = inVals ++ inVars
    //val kernelContext = getEffectsKernel(sym, rhs)
    val kernelContext = getEffectsBlock(sym) //ifGenAgree( _.getEffectsBlock(sym), true )
    val inMutating = (inputs flatMap { mutating(kernelContext, _) }).distinct

    // additional data deps: for each of my inputs, look at the kernels already generated and see if any of them
    // mutate it, and if so, add that kernel as a data-dep
    val extraDataDeps = (kernelMutatingDeps filter { case (s, mutates) => (!(inputs intersect mutates).isEmpty) }).keys
    val inControlDeps = (controlDeps ++ extraDataDeps).distinct

    // anti deps: for each of my mutating inputs, look at the kernels already generated and see if any of them
    // read it, add that kernel as an anti-dep
    val antiDeps = (kernelInputDeps filter { case (s, in) => (!(inMutating intersect in).isEmpty) }).keys.toList

    // add this kernel to global generated state
    sym foreach { s => kernelInputDeps += { s -> inputs } }
    sym foreach { s => kernelMutatingDeps += { s -> inMutating } }

    // debug
    /*
    stream.println("inputs: " + inputs)
    stream.println("mutating inputs: " + inMutating)
    stream.println("extra data deps: " + extraDataDeps)
    stream.println("control deps: " + inControlDeps)
    stream.println("anti deps:" + antiDeps)
    */
    println(outputSlotTypes)

    // emit task graph node
    rhs match { 
      case op: AbstractFatLoop => 
        emitMultiLoop(kernelName, outputs, inputs, inControlDeps, antiDeps, op.body.exists(_.isInstanceOf[DeliteReduceElem[_]]))
      case ThinDef(z) => z match {
        case s:DeliteOpSingleTask[_] => emitSingleTask(kernelName, outputs, inputs, inControlDeps, antiDeps)
        case op:AbstractLoop[_] => emitMultiLoop(kernelName, outputs, inputs, inControlDeps, antiDeps, op.body.isInstanceOf[DeliteReduceElem[_]])
        case m:DeliteOpMap[_,_,_] => emitMap(kernelName, outputs, inputs, inControlDeps, antiDeps)
        case r:DeliteOpReduce[_] => emitReduce(kernelName, outputs, inputs, inControlDeps, antiDeps)
        case a:DeliteOpMapReduce[_,_,_] => emitMapReduce(kernelName, outputs, inputs,inControlDeps, antiDeps)
        case z:DeliteOpZipWith[_,_,_,_] => emitZipWith(kernelName, outputs, inputs, inControlDeps, antiDeps)
        case f:DeliteOpForeach[_,_] => emitForeach(kernelName, outputs, inputs, inControlDeps, antiDeps)
        case _ => emitSingleTask(kernelName, outputs, inputs, inControlDeps, antiDeps) // things that are not specified as DeliteOPs, emit as SingleTask nodes
      }
    }

    // whole program gen (for testing)
    //emitValDef(sym, "embedding.scala.gen.kernel_" + quote(sym) + "(" + inputs.map(quote(_)).mkString(",") + ")")
  }

  /**
   * @param sym         the symbol representing the kernel
   * @param inputs      a list of real kernel dependencies (formal kernel parameters)
   * @param controlDeps a list of control dependencies (must execute before this kernel)
   * @param antiDeps    a list of WAR dependencies (need to be committed in program order)
   */
  def emitSingleTask(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"SingleTask\"")
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }

  def emitMultiLoop(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]], needsCombine: Boolean)
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"MultiLoop\", \"needsCombine\":" + needsCombine)
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }
  
  def emitMap(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Map\"")
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }

  def emitReduce(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Reduce\"")
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }

  def emitMapReduce(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"MapReduce\"")
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }

  def emitZipWith(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"ZipWith\"")
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }

  def emitForeach(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Foreach\"")
    emitOpCommon(id, outputs, inputs, controlDeps, antiDeps)
  }

  def emitOpCommon(id: String, outputs: List[Exp[_]], inputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print(" , \"kernelId\" : \"" + id + "\" ")
    stream.print(" , \"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "],\n")
    val outputsStr = if(outputs.isEmpty) "" else outputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"outputs\":[" + outputsStr + "],\n")
    val inputsStr = if(inputs.isEmpty) "" else inputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"inputs\":[" + inputsStr + "],\n")
    val controlDepsStr = if(controlDeps.isEmpty) "" else controlDeps.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"controlDeps\":[" + controlDepsStr + "],\n")
    val antiDepsStr = if(antiDeps.isEmpty) "" else antiDeps.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"antiDeps\":[" + antiDepsStr + "],\n")
    val metadataStr = if (metadata.isEmpty) "" else metadata.mkString(",")
    stream.print("  \"metadata\":{" + metadataStr + "},\n")
    val returnTypesStr = if(returnTypes.isEmpty) "" else returnTypes.mkString(",")
    stream.print("  \"return-types\":{" + returnTypesStr + "}")

    if (!outputSlotTypes.isEmpty) {
      stream.print(",\n")
      val rts = for (s <- outputs) yield {
        val str = quote(s)
        "  \""+str+"\":{" + outputSlotTypes(str).mkString(",") + "}"
      }
      stream.print("\"output-types\":{" + rts.mkString(",") + "}\n")
    } else
      stream.print("\n")
    
    stream.println("},")
  }

  def nop = throw new RuntimeException("Not Implemented Yet")

}
