package ppl.delite.framework.codegen.delite.generators

import ppl.delite.framework.codegen.delite.DeliteCodegen
import collection.mutable.{ArrayBuffer, ListBuffer}
import java.io.{StringWriter, FileWriter, File, PrintWriter}
import ppl.delite.framework.{Util, Config}
import scala.virtualization.lms.internal.{GenerationFailedException, ScalaGenEffect, GenericCodegen}
import ppl.delite.framework.ops.{VariantsOpsExp, DeliteOpsExp}

trait DeliteGenTaskGraph extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  private def vals(sym: Sym[_]) : List[Sym[_]] = sym match {
    case Def(Reify(s, effects)) => if (s.isInstanceOf[Sym[_]]) List(s.asInstanceOf[Sym[_]]) else Nil
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
      case Def(Mutation(x,effects)) => if (syms(x) contains sym) List(sym):List[Sym[_]] else Nil
      case _ => Nil: List[Sym[_]]
    }

  // this is all quite unfortunate
  private def appendScope() = {
    (emittedNodes flatMap { e => if (findDefinition(e).isDefined) List(findDefinition(e).get.asInstanceOf[TP[Any]]) else Nil }) ::: scope
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) : Unit = {
    assert(generators.length >= 1)

    var resultIsVar = false
    var skipEmission = false
    var nestedNode: TP[_] = null

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

    implicit val supportedTargets = new ListBuffer[String]
    implicit val returnTypes = new ListBuffer[Pair[String, String]]
    implicit val metadata = new ArrayBuffer[Pair[String, String]]

    // parameters for delite overrides
    deliteInputs = (inVals ++ inVars)
    deliteResult = Some(sym) //findDefinition(rhs) map { _.sym }

    if (!skipEmission) {
      for (gen <- generators) {
        val buildPath = Config.buildDir + java.io.File.separator + gen + java.io.File.separator
        val outDir = new File(buildPath); outDir.mkdirs()
        val outFile = new File(buildPath + quote(sym) + "." + gen.kernelFileExt)
        val kstream = new PrintWriter(outFile)
        val bodyString = new StringWriter()
        val bodyStream = new PrintWriter(bodyString)

        try{
          rhs match {
            case op:DeliteOp[_] => deliteKernel = true
            case _ => deliteKernel = false
          }

          //initialize
          gen.kernelInit(sym, inVals, inVars, resultIsVar)

          // emit kernel to bodyStream //TODO: must kernel body be emitted before kernel header?
          gen.emitNode(sym, rhs)(bodyStream)
          bodyStream.flush

          val resultType = if (gen.toString == "scala") {
            rhs match {
              case map: DeliteOpMap[_,_,_] => "generated.scala.DeliteOpMap[" + gen.remap(map.v.Type) + "," + gen.remap(map.func.Type) + "," + gen.remap(map.alloc.Type) + "]"
              case zip: DeliteOpZipWith[_,_,_,_] => "generated.scala.DeliteOpZipWith[" + gen.remap(zip.v._1.Type) + "," + gen.remap(zip.v._2.Type) + "," + gen.remap(zip.func.Type) + "," + gen.remap(zip.alloc.Type) +"]"
              case red: DeliteOpReduce[_] => "generated.scala.DeliteOpReduce[" + gen.remap(red.func.Type) + "]"
              case mapR: DeliteOpMapReduce[_,_,_] => "generated.scala.DeliteOpMapReduce[" + gen.remap(mapR.mV.Type) + "," + gen.remap(mapR.reduce.Type) + "]"
              case foreach: DeliteOpForeach[_,_] => "generated.scala.DeliteOpForeach[" + gen.remap(foreach.v.Type) + "]"
              case _ => gen.remap(sym.Type)
            }
          } else gen.remap(sym.Type)

          // emit kernel
          gen.emitKernelHeader(sym, inVals, inVars, resultType, resultIsVar)(kstream)
          kstream.println(bodyString.toString)
          gen.emitKernelFooter(sym, inVals, inVars, resultType, resultIsVar)(kstream)

          //record that this kernel was successfully generated
          supportedTargets += gen.toString
          if (resultIsVar) {
            returnTypes += new Pair[String,String](gen.toString,"generated.scala.Ref[" + gen.remap(sym.Type) + "]") {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
            }
          }
          else {
            returnTypes += new Pair[String,String](gen.toString,gen.remap(sym.Type)) {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
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
          case e:GenerationFailedException => // no generator found
            gen.exceptionHandler(e, outFile, kstream)
            //e.printStackTrace
            if(gen.nested > 1) {
              nestedNode = gen.lastNodeAttempted
            }
          case e:Exception => throw(e)
        }
      }
    }

    if (skipEmission == false && supportedTargets.isEmpty) {
      var msg = "Node " + quote(sym) + "[" + rhs + "] could not be generated by any code generator"
      if(nested > 1) msg = "Failure is in nested node " + quote(nestedNode.sym) + "[" + nestedNode.rhs + "]. " + msg
      system.error(msg)
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
      case c:DeliteOpCondition[_] => emitIfThenElse(c.cond, c.thenp, c.elsep, sym, inputs, inMutating, inControlDeps, antiDeps)
      case w:DeliteOpWhileLoop => emitWhileLoop(w.cond, w.body, sym, inputs, inMutating, inControlDeps, antiDeps)
      case s:DeliteOpSingleTask[_] => emitSingleTask(sym, inputs, inMutating, inControlDeps, antiDeps)
      case m:DeliteOpMap[_,_,_] => emitMap(sym, rhs, inputs, inMutating, inControlDeps, antiDeps)
      case r:DeliteOpReduce[_] => emitReduce(sym, inputs, inMutating, inControlDeps, antiDeps)
      case a:DeliteOpMapReduce[_,_,_] => emitMapReduce(sym, rhs, inputs, inMutating, inControlDeps, antiDeps)
      case z:DeliteOpZipWith[_,_,_,_] => emitZipWith(sym, inputs, inMutating, inControlDeps, antiDeps)
      case f:DeliteOpForeach[_,_] => emitForeach(sym, rhs, inputs, inMutating, inControlDeps, antiDeps)
      case _ => emitSingleTask(sym, inputs, inMutating, inControlDeps, antiDeps) // things that are not specified as DeliteOPs, emit as SingleTask nodes
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
  def emitSingleTask(sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"SingleTask\"")
    emitExecutionOpCommon(sym, inputs, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitMap(sym: Sym[_], rhs: Def[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Map\"")
    emitExecutionOpCommon(sym, inputs, mutableInputs, controlDeps, antiDeps)

    //// constructing from variant encoded in the IR
    val output = rhs match {
      case map:DeliteOpMap[_,_,_] => map.alloc
    }
    emitVariant(sym, rhs, output, inputs, mutableInputs, controlDeps, antiDeps)

    stream.println("},")
  }

  def emitReduce(sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Reduce\"")
    emitExecutionOpCommon(sym, inputs, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitMapReduce(sym: Sym[_], rhs: Def[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"MapReduce\"")
    emitExecutionOpCommon(sym, inputs, mutableInputs, controlDeps, antiDeps)

    //// constructing from variant encoded in the IR
    val output = rhs match {
      case mapR:DeliteOpMapReduce[_,_,_] => mapR.alloc
    }
    emitVariant(sym, rhs, output, inputs, mutableInputs, controlDeps, antiDeps)

    stream.println("},")
  }

  def emitZipWith(sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"ZipWith\"")
    emitExecutionOpCommon(sym, inputs, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitForeach(sym: Sym[_], rhs: Def[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Foreach\"")
    emitExecutionOpCommon(sym, inputs, mutableInputs, controlDeps, antiDeps)

    //// constructing from variant encoded in the IR
    val output = rhs match {
      case vw:DeliteOpMapLikeWhileLoopVariant => vw.alloc
    }
    emitVariant(sym, rhs, output, inputs, mutableInputs, controlDeps, antiDeps)

    stream.println("},")
  }

  def emitIfThenElse(cond: Exp[Boolean], thenp: Exp[_], elsep: Exp[_], sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Conditional\",")
    stream.println("  \"outputId\" : \"" + quote(sym) + "\",")
    emitExp("cond", cond)
    emitExp("then", thenp)
    emitExp("else", elsep)
    stream.println("  \"condOutput\": \"" + quote(getBlockResult(cond)) + "\",")
    stream.println("  \"thenOutput\": \"" + quote(getBlockResult(thenp)) + "\",")
    stream.println("  \"elseOutput\": \"" + quote(getBlockResult(elsep)) + "\",")
    stream.println("  \"controlDeps\":[" + makeString(controlDeps) + "],")
    stream.println("  \"antiDeps\":[" + makeString(antiDeps) + "],")
    if (remap(thenp.Type) != remap(elsep.Type))
      throw new RuntimeException("Delite conditional with different then and else return types")
    val returnTypesStr = if(returnTypes.isEmpty) "" else returnTypes.mkString(",")
    stream.println("  \"return-types\":{" + returnTypesStr + "}")
    stream.println("},")
  }

  def emitWhileLoop(cond: Exp[Boolean], body: Exp[Unit], sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.println("{\"type\":\"WhileLoop\",")
    stream.println("  \"outputId\" : \"" + quote(sym) + "\",")
    emitExp("cond", cond)
    emitExp("body", body)
    stream.println("  \"condOutput\": \"" + quote(getBlockResult(cond)) + "\",")
    //stream.println("  \"bodyOutput\": \"" + quote(getBlockResult(body)) + "\",")
    stream.println("  \"controlDeps\":[" + makeString(controlDeps) + "],")
    stream.println("  \"antiDeps\":[" + makeString(antiDeps) + "]")
    stream.println("},")
  }

  def emitControlFlowOpCommon(sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
  }

  def emitExecutionOpCommon(sym: Sym[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print(" , \"kernelId\" : \"" + quote(sym) + "\" ")
    stream.print(" , \"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "],\n")
    val inputsStr = if(inputs.isEmpty) "" else inputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"inputs\":[" + inputsStr + "],\n")
    val mutableInputsStr = if(mutableInputs.isEmpty) "" else mutableInputs.map(quote(_)).mkString("\"","\",\"","\"")
    stream.print("  \"mutableInputs\":[" + mutableInputsStr + "],\n")
    emitDepsCommon(controlDeps, antiDeps)
    val metadataStr = if (metadata.isEmpty) "" else metadata.mkString(",")
    stream.print("  \"metadata\":{" + metadataStr + "},\n")
    val returnTypesStr = if(returnTypes.isEmpty) "" else returnTypes.mkString(",")
    stream.print("  \"return-types\":{" + returnTypesStr + "}\n")
  }


  def emitDepsCommon(controlDeps: List[Exp[_]], antiDeps: List[Exp[_]], last:Boolean = false)(implicit stream: PrintWriter) {
    stream.print("  \"controlDeps\":[" + makeString(controlDeps) + "],\n")
    stream.print("  \"antiDeps\":[" + makeString(antiDeps) + "]" + (if(last) "\n" else ",\n"))
  }

  def emitVariant(sym: Sym[_], rhs: Def[_], output: Exp[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
                 (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) {

    if (!rhs.isInstanceOf[Variant[_]] || nested > Config.nestedVariantsLevel) return

    // pre
    val saveMutatingDeps = kernelMutatingDeps
    val saveInputDeps = kernelInputDeps
    kernelMutatingDeps = Map()
    kernelInputDeps = Map()
    stream.print(",\"variant\": {")

    // variant
    rhs match {
      case vw:DeliteOpMapLikeWhileLoopVariant => emitMapLikeWhileLoopVariant(vw, sym, output, inputs, mutableInputs, controlDeps, antiDeps)
      case _ =>
    }

    // post
    emitEOV()
    emitOutput(output)
    stream.println("}")
    kernelInputDeps = saveInputDeps
    kernelMutatingDeps = saveMutatingDeps
    prependInputs = Nil
  }

  def emitMapLikeWhileLoopVariant(vw: DeliteOpMapLikeWhileLoopVariant, sym: Sym[_], output: Exp[_], inputs: List[Exp[_]], mutableInputs: List[Exp[_]], controlDeps: List[Exp[_]], antiDeps: List[Exp[_]])
    (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) {

    val save = scope
    stream.print("\"ops\":[" )
    emitBlock(vw.alloc)
    scope = appendScope()
    // we should not be reifying during code-gen, see todo in DeliteOps.scala
    emitBlock(reifyEffects(vw.index))
    //emitBlock(vw.index)
    emitWhileLoop(vw.cond, vw.body, sym, inputs, mutableInputs, controlDeps, antiDeps)
    scope = save
  }

  def emitOutput(x: Exp[_])(implicit stream: PrintWriter) = {
    emitExp("output", getBlockResult(x))
    //stream.print("  \"output\": \"")
    //stream.print(emitExp(getBlockResult(x)))
    //stream.print("\"\n")
  }

  def emitEOV()(implicit stream: PrintWriter) = {
    stream.print("{\"type\":\"EOV\"}\n],\n")
  }

  def emitExp(prefix: String, e: Exp[Any])(implicit stream: PrintWriter) = e match {
    case c:Const[Any] => stream.println("  \"" + prefix + "Type\": \"const\",")
                         stream.println("  \"" + prefix + "Value\": \"" + quote(e) + "\",")
    case s:Sym[Any] =>  stream.println("  \"" + prefix + "Type\": \"symbol\",")
                        stream.println("  \"" + prefix + "Ops\": [")
                        val saveMutatingDeps = kernelMutatingDeps
                        val saveInputDeps = kernelInputDeps
                        kernelMutatingDeps = Map()
                        kernelInputDeps = Map()
                        emitBlock(e)
                        emitEOV()
                        kernelInputDeps = saveInputDeps
                        kernelMutatingDeps = saveMutatingDeps
  }

  private def makeString(list: List[Exp[_]]) = {
    if(list.isEmpty) "" else list.map(quote(_)).mkString("\"","\",\"","\"")
  }

  // more quirks
  override def quote(x: Exp[_]) = x match {
    case r:Reify[_] => quote(r.x)
    case _ => super.quote(x)
  }

  def nop = throw new RuntimeException("Not Implemented Yet")

}
