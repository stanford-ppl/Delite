package ppl.delite.framework.codegen.delite.generators

import collection.mutable.{ArrayBuffer, ListBuffer, HashMap}
import java.io.{StringWriter, FileWriter, File, PrintWriter}
import scala.virtualization.lms.common.LoopFusionOpt
import scala.virtualization.lms.internal.{GenericCodegen, CLikeCodegen, GPUCodegen, ScalaCodegen, GenerationFailedException}
import scala.virtualization.lms.internal.Targets._
import ppl.delite.framework.ops.DeliteCollection
import scala.reflect.SourceContext

import ppl.delite.framework.codegen.delite.{DeliteCodegen, DeliteKernelCodegen}
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.transform.LoopSoAOpt
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.analysis.{NestedLoopMappingAnalysis,StencilAnalysis}
import ppl.delite.framework.datastructures.DeliteArrayFatExp

trait DeliteGenTaskGraph extends DeliteCodegen with DeliteKernelCodegen with LoopFusionOpt with LoopSoAOpt {
  val IR: DeliteOpsExp
  import IR.{ __newVar => _, __assign => _, __ifThenElse => _ , _ }

  private def mutating(kernelContext: State, sym: Sym[Any]) : List[Sym[Any]] = kernelContext flatMap {
    case Def(Reflect(x,u,effects)) => if (u.mayWrite contains sym) List(sym) else Nil
    case _ => Nil
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = emitAnyNode(List(sym), rhs)
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = emitAnyNode(sym, rhs)

  var effectWrapper: Option[Sym[Any]] = None
  private def withEffectWrapper(r: Sym[Any])(block: => Unit) = {
    effectWrapper = Some(r)
    block
    effectWrapper = None
  }

  private def emitAnyNode(sym: List[Sym[Any]], rhs: Any): Unit = {
    assert(generators.length >= 1)

    printlog("DeliteGenTaskGraph.emitNode "+sym+"="+rhs)

    val kernelName = sym.map(quote).mkString("")

    val skipEmission = rhs match {
      case c:DeliteOpCondition[_] => true //we want to unroll these in the DEG rather than generate a single kernel
      case w:DeliteOpWhileLoop => true
      case _ => false
    }
      
    val resultIsVar = rhs match { case NewVar(_) => true case _ => false }
    val external = rhs.isInstanceOf[DeliteOpExternal[_]]

    // we will try to generate any node that is not purely an effect node
    rhs match {
      case Reflect(s, u, effects) =>
        //controlDeps = effects; // <---  now handling control deps here...!! <--- would like to, but need to hand *precise* schedule to runtime
        withEffectWrapper(sym(0)) {
          super.emitNode(sym(0), rhs.asInstanceOf[Def[Any]])
        }
        return
      case Reify(s, u, effects) =>
        //controlDeps = effects
        withEffectWrapper(sym(0)) {
          super.emitNode(sym(0), rhs.asInstanceOf[Def[Any]])
        }
        return
      case _ => // continue and attempt to generate kernel
    }


    implicit val supportedTargets = new ListBuffer[String]
    implicit val returnTypes = new ListBuffer[Pair[String, String]]
    implicit val outputSlotTypes = new HashMap[String, ListBuffer[(String, String)]]
    implicit val metadata = new ArrayBuffer[Pair[String, String]]

    // parameters for delite overrides
    val inVars = inputVars(rhs)
    deliteInputs = (inputVals(rhs) ++ inVars)
    deliteResult = Some(sym) //findDefinition(rhs) map { _.sym }

    /**
     * Run stencil analysis locally on the current op to insure that all transformations, including fusion, have already happened.
     */
    val stencilAnalysis = new StencilAnalysis { val IR: DeliteGenTaskGraph.this.IR.type = DeliteGenTaskGraph.this.IR }
    stencilAnalysis.innerScope = this.innerScope

    def gatherStencil(sym: List[Sym[Any]], rhs: Any) = rhs match {
      case SimpleFatLoop(sz,v,body) =>
        for ((s,d) <- sym.zip(body)) {
          stencilAnalysis.process(s,v,d)
        }
      case SimpleLoop(sz,v,body) =>
        stencilAnalysis.process(sym(0),v,body)
      case _ =>
    }

    gatherStencil(sym,rhs)
    allStencils = /*allStencils ++*/ stencilAnalysis.getLoopStencils

    // Run GPU multi-dim mapping analysis
    if(Config.enableGPUMultiDim) {
      val loopAnalysis = new NestedLoopMappingAnalysis { val IR: DeliteGenTaskGraph.this.IR.type = DeliteGenTaskGraph.this.IR }
      loopAnalysis.innerScope = this.innerScope

      val runAnalysis = rhs match {
        case SimpleFatLoop(_,_,_) | SimpleLoop(_,_,_) => true
        case _: AbstractLoop[_] => true
        case Reflect(_:AbstractLoop[_],u,es) => true
        case _ => false
      }
      if (runAnalysis) {
        loopAnalysis.start(sym, rhs, deliteInputs)
        loopAnalysis.printResult(sym)
      }
    }

    class JsonPair(_1: String, _2: String) extends Pair(_1,_2) {
      override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
    }

    //compute result types for all syms for all targets independently of whether or not the kernel itself could be generated
    for (gen <- generators) {
      var genReturnType: String = null
      val tpes = for (s <- sym) {
        try {
          val tpeStr = gen match {
            case g:ScalaCodegen if resultIsVar => "generated.scala.Ref[" + g.remap(s.tp) + "]"
            case g:CLikeCodegen if resultIsVar && cppMemMgr == "recnt" => g.wrapSharedPtr(g.deviceTarget.toString + "Ref" + g.unwrapSharedPtr(g.remap(sym.head.tp)))
            case g:CLikeCodegen if resultIsVar => g.deviceTarget.toString + "Ref" + g.remap(sym.head.tp)
            case g => g.remap(s.tp)
          }
          genReturnType = tpeStr
          outputSlotTypes.getOrElseUpdate(quote(s), new ListBuffer) += new JsonPair(gen.toString, tpeStr)
        }
        catch {
          case e:GenerationFailedException => //
        }
      }

      if (genReturnType != null) {
        val retStr = if (hasOutputSlotTypes(rhs)) "activation_" + kernelName else genReturnType
        returnTypes += new JsonPair(gen.toString, retStr)
      }
    }

    if (!skipEmission) for (gen <- generators) {
      val sep = java.io.File.separator
      //NOTE: GPU targets generate only device functions of multiloops at compile-time,
      //      and we put those functions in 'device' directory to make compilation a bit easier at walk-time
      val buildPath = gen match {
        case g: GPUCodegen if (hasOutputSlotTypes(rhs)) => Config.buildDir + sep + gen + sep + "kernels" + sep + "device" + sep
        case _ => Config.buildDir + sep + gen + sep + "kernels" + sep
      }
      val outDir = new File(buildPath); outDir.mkdirs()
      val outFile = new File(buildPath + kernelName + "." + gen.fileExtension)
      val kStream = new PrintWriter(outFile)
      try { 
        gen.withStream(kStream){ gen.emitKernel(sym, rhs) } 
        kStream.close()

        // record that this kernel was successfully generated
        supportedTargets += gen.toString

        //add MetaData
        if (gen.hasMetaData) {
          metadata += new Pair(gen.toString, gen.getMetaData) {
            override def toString = "\"" + _1 + "\" : " + _2 //note slightly different than standard JsonPair
          }
        }

      } catch {
        case e: GenerationFailedException => // no generator found
          kStream.close()
          outFile.delete()
          if (Config.dumpException) {
            System.err.println(gen.toString + ":" + (sym.map(quote)))
            e.printStackTrace
          }
          if (Config.strictGeneration(gen.toString, e)) throw e
      }
    }

    if (!skipEmission && supportedTargets.isEmpty) {
      var msg = "Node " + kernelName + "[" + rhs + "] could not be generated by any code generator"
      sys.error(msg)
    }

    val outputs = sym
    val inputs = deliteInputs

    // visible kernel effects
    val outerKernelEffects = effectWrapper match {
      case Some(Def(Reflect(x,u,es))) => List(effectWrapper.get)
      case Some(Def(Reify(x,u,es))) => es
      case _ => Nil
    }
    // internal kernel effects (that might write to free variables)
    // ideally these would always be propagated up and internalKernelEffects should not be needed,
    // but i don't think that's always happening now... so this is effectively a safety net
    val defs = rhs match {
      case op:AbstractFatLoop => op.body
      case op:AbstractFatIfThenElse => (op.thenp zip op.elsep) map (p => IfThenElse(op.cond,p._1,p._2))
      case d: Def[Any] => List(d)
    }
    val internalKernelEffects = getEffectsBlock(defs)

    val kernelContext = outerKernelEffects ++ internalKernelEffects

    // kernel inputs mutated by any visible effectful operation inside kernel
    val inMutating = (inputs flatMap { mutating(kernelContext, _) }).distinct

    // additional data deps: for each of my inputs, look at the kernels already generated and see if any of them
    // mutate it, and if so, add that kernel as a data-dep
    val extraDataDeps = (kernelMutatingDeps filter { case (s, mutates) => (!(inputs intersect mutates).isEmpty) }).keys
    val inControlDeps = (controlDeps ++ extraDataDeps).distinct

    // anti deps: for each of my mutating inputs, look at the kernels already generated and see if any of them
    // read it, add that kernel as an anti-dep
    val antiDeps = (effectKernelReads filter { case (s, in) => (!(inMutating intersect in).isEmpty) }).keys.toList

    // add this kernel to global generated state
    sym collect { case s@Def(Reflect(x,u,es)) => effectKernelReads += { s -> (u.mayRead ++ u.mstRead).distinct } }
    sym foreach { s => kernelMutatingDeps += { s -> inMutating } }

    // debug
    /*
    stream.println("inputs: " + inputs)
    stream.println("mutating inputs: " + inMutating)
    stream.println("extra data deps: " + extraDataDeps)
    stream.println("control deps: " + inControlDeps)
    stream.println("anti deps:" + antiDeps)
    */
    printlog(outputSlotTypes)

    val optContext = sym.find(!_.sourceContexts.isEmpty).map(_.sourceContexts.head)

    /**
     * Domain-specific inputs to loops cause issues with the stencil analysis, since it only records accesses on arrays.
     * This method is intended to allow us to associate structs with their component arrays by identifying array symbols
     * that are fields within a struct. However, discovering the original array symbol requires some care since each
     * field read is a new symbol.
     *
     * Until we implement this, we can only associate the stencil analysis results with an op's input
     * if input structs are unwrapped.
     */
    /*
    def getArrayInputs(s: Exp[Any]): Seq[Exp[Any]] = s.tp match {
      case StructType(tag,elems) =>
        Predef.println("found struct with elems " + elems)
        elems.flatMap(e => getArrayInputs(e._2))
      case y if y.erasure == manifest[DeliteArray[Any]].erasure => List(s)
      case _ =>
        Predef.println("did not find struct or array: " + findDefinition(s.asInstanceOf[Sym[Any]]).toString)
        Nil
    }
    */

    // result is a single stencil representing all the info we have for this op's inputs
    val opStencil = if (sym == Nil) new Stencil() else sym.map(i => allStencils.getOrElse(i, new Stencil())).reduce((a,b) => a ++ b)

    // compute aliases

    // We need to use a special flag to propagate DeliteStruct "containsSyms" aliases, which are generally turned off to suppress nested mutable errors.
    // See DeliteStructs.scala containsSyms for more info.
    val saveDeliteStructAliases = _deliteStructAliases
    _deliteStructAliases = true
    val aliases = allAliases(rhs,deliteStructAliasMode)
    // println("aliases for " + sym + "(" + rhs.toString + "): " + aliases)
    _deliteStructAliases = saveDeliteStructAliases

    // emit task graph node
    rhs match {
      case op: AbstractFatLoop =>
        // Predef.println("emitting DeliteFatLoop (" + sym + "), inputs are: ")
        // Predef.println(inputs)
        // Predef.println("stencil is: " + opStencil)
        // val arrayInputs = inputs.flatMap(getArrayInputs)
        // Predef.println("  array inputs are: ")
        // Predef.println(arrayInputs)val i = (a match {
        emitMultiLoop(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, aliases, op.size, loopBodyAverageDynamicChunks(op.body), op.body.exists (loopBodyNeedsCombine _), op.body.exists (loopBodyNeedsPostProcess _), optContext, opStencil)
      case op: AbstractFatIfThenElse =>
        assert(sym.length == 1, "TODO: implement fat if then else")
        emitIfThenElse(Block(op.cond), op.thenp.head, op.elsep.head, kernelName, outputs, resultIsVar, inputs, inMutating, inControlDeps, antiDeps, optContext)
      case z =>
        z match {
          //case op:DeliteOpLoop[_] =>
          case op:AbstractLoop[_] =>
            // Predef.println("emitting DeliteLoop (" + sym + "), inputs are: ")
            // Predef.println(inputs)
            // Predef.println("stencil is: " + opStencil)
            emitMultiLoop(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, aliases, op.size, loopBodyNumDynamicChunks(op.body), loopBodyNeedsCombine(op.body), loopBodyNeedsPostProcess(op.body), optContext, opStencil)
          case e:DeliteOpExternal[_] => emitExternal(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, aliases)
          case c:DeliteOpCondition[_] => emitIfThenElse(Block(c.cond), c.thenp, c.elsep, kernelName, outputs, resultIsVar, inputs, inMutating, inControlDeps, antiDeps, optContext)
          case w:DeliteOpWhileLoop => emitWhileLoop(w.cond, w.body, kernelName, outputs, resultIsVar, inputs, inMutating, inControlDeps, antiDeps, optContext)
          case s:DeliteOpSingleTask[_] => emitSingleTask(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, aliases, optContext)
          case _ => emitSingleTask(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, aliases, if (outputs(0).sourceContexts.isEmpty) None else Some(outputs(0).sourceContexts.head)) // things that are not specified as DeliteOPs, emit as SingleTask nodes
        }
    }

    // whole program gen (for testing)
    //emitValDef(sym, "embedding.scala.gen.kernel_" + quote(sym) + "(" + inputs.map(quote(_)).mkString(",") + ")")
  }

  /**
   * DEG quotes - handles descrepancies between quoting constants in code vs. in the DEG
   */
  override def quote(x: Exp[Any]) = x match {
    case Const(s: String) => super.quote(x).replaceAllLiterally("\"","\\\"") //escape constant quoted strings
    case Const(l: Long) => super.quote(x).dropRight(1) //drop the "L" suffix
    case _ => super.quote(x)
  }

  /**
   * @param sym         the symbol representing the kernel
   * @param inputs      a list of real kernel dependencies (formal kernel parameters)
   * @param controlDeps a list of control dependencies (must execute before this kernel)
   * @param antiDeps    a list of WAR dependencies (need to be committed in program order)
   */

  def emitMultiLoop(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], aliases: List[Exp[Any]], size: Exp[Int], numDynamicChunks: Int, needsCombine: Boolean, needsPostProcess: Boolean,
                    sourceContext: Option[SourceContext], stencil: Stencil)
       (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
   stream.println("{\"type\":\"MultiLoop\",")
   emitSourceContext(sourceContext, stream, id)
   stream.println(",\n")
   emitConstOrSym(size, "size")
   stream.println(",\n")
   emitConstOrSym(Const[Int](numDynamicChunks), "numDynamicChunks")
   stream.print(",\"needsCombine\":" + needsCombine)
   stream.println(",\"needsPostProcess\":" + needsPostProcess)
   emitStencil(inputs, stencil)
   emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps, aliases)
   stream.println("},")
  }

  def emitExternal(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], aliases: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"External\"")
    // TODO: add thread control, other configuration?
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps, aliases)
    stream.println("},")
  }

  def emitSingleTask(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], aliases: List[Exp[Any]], sourceContext: Option[SourceContext])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"SingleTask\",")
    emitSourceContext(sourceContext, stream, id)
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps, aliases)
    stream.println("},")
  }

  def emitForeach(rhs: Def[Any], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], aliases: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Foreach\"")
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps, aliases)
    stream.println("},")
  }

  def emitIfThenElse(cond: Block[Boolean], thenp: Block[Any], elsep: Block[Any], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], sourceContext: Option[SourceContext])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Conditional\",")
    emitSourceContext(sourceContext, stream, id)
    stream.println(",\n")
    stream.println("  \"outputId\" : \"" + id + "\",")
    emitSubGraph("cond", cond)
    emitSubGraph("then", thenp)
    emitSubGraph("else", elsep)
    stream.println("  \"condOutput\": \"" + quote(getBlockResult(cond)) + "\",")
    stream.println("  \"thenOutput\": \"" + quote(getBlockResult(thenp)) + "\",")
    stream.println("  \"elseOutput\": \"" + quote(getBlockResult(elsep)) + "\",")
    stream.println("  \"controlDeps\":[" + makeString(controlDeps) + "],")
    stream.println("  \"antiDeps\":[" + makeString(antiDeps) + "],")
    if (remap(thenp.tp) != remap(elsep.tp))
      throw new RuntimeException("Delite conditional with different then and else return types: " + thenp + ", " + remap(thenp.tp) + " and " + elsep + ", " + remap(elsep.tp))
    if (thenp.tp == manifest[Any]) System.err.println("[delite] WARNING: returning type Any from IfThenElse is only supported in Scala")

    stream.println("  \"return-types\":{" + returnTypes.mkString(",") + "}")
    stream.println("},")
  }

  def emitWhileLoop(cond: Block[Boolean], body: Block[Unit], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], sourceContext: Option[SourceContext])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.println("{\"type\":\"WhileLoop\",")
    emitSourceContext(sourceContext, stream, id)
    stream.println(",\n")
    stream.println("  \"outputId\" : \"" + id + "\",")
    emitSubGraph("cond", cond)
    emitSubGraph("body", body)
    stream.println("  \"condOutput\": \"" + quote(getBlockResult(cond)) + "\",")
    //stream.println("  \"bodyOutput\": \"" + quote(getBlockResult(body)) + "\",")
    stream.println("  \"controlDeps\":[" + makeString(controlDeps) + "],")
    stream.println("  \"antiDeps\":[" + makeString(antiDeps) + "]")
    stream.println("},")
  }

  def emitControlFlowOpCommon(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
  }

  def emitExecutionOpCommon(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], aliases: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {

    stream.print(" , \"kernelId\" : \"" + id + "\" ")
    stream.print(" , \"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "],\n")
    stream.print("  \"outputs\":[" + outputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    stream.print("  \"inputs\":[" + inputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    //stream.print("  \"input-types\":{" + inputs.map(s => "\"" + quote(s) + "\":{" + remapAllTargets(s).mkString(",") + "}").mkString(",") + "},\n")
    stream.print("  \"mutableInputs\":[" + mutableInputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    emitDepsCommon(controlDeps, antiDeps)
    val metadataStr = if (metadata.isEmpty) "" else metadata.mkString(",")
    stream.print("  \"metadata\":{" + metadataStr + "},\n")
    val returnTypesStr = if(returnTypes.isEmpty) "" else returnTypes.mkString(",")
    stream.println("  \"return-types\":{" + returnTypesStr + "},")
    stream.println("  \"output-types\":{" + outputs.map(s => "\"" + quote(s) + "\":{" + outputSlotTypes(quote(s)).mkString(",") + "}").mkString(",") +  "},")
    stream.println("  \"aliases\":[" + makeString(aliases) + "]")
  }

  def emitDepsCommon(controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], last:Boolean = false) {
    stream.print("  \"controlDeps\":[" + makeString(controlDeps) + "],\n")
    stream.print("  \"antiDeps\":[" + makeString(antiDeps) + "]" + (if(last) "\n" else ",\n"))
  }

  def emitStencil(inputs: List[Exp[Any]], stencil: Stencil) = {
    stream.print(" , \"stencil\":{")
    val stencilMap = inputs map { i =>
      "\"" + quote(i) + "\":\"" + (stencil.get(i) match {
        case Some(Interval(mult,stride,len)) => "range(" + quote(mult) + ", " + quote(stride) + ", " + quote(len) + ")"
        case Some(Constant(i)) => "const(" + quote(i) + ")"
        case Some(z) => z.toString
        case None => "none"
      }) + "\""
    }
    stream.print(stencilMap.mkString(","))
    stream.println("}")
  }

  def emitConstOrSym(x: Exp[Any], prefix: String) = x match {
    case c:Const[Any] => stream.println("  \"" + prefix + "Type\": \"const\",")
                         stream.println("  \"" + prefix + "Value\": \"" + quote(x) + "\"")
    case s:Sym[Any] =>   stream.println("  \"" + prefix + "Type\": \"symbol\",")
                         stream.println("  \"" + prefix + "Value\": \"" + quote(getBlockResult(Block(x))) + "\"") // x might be a Reify
  }

  def emitOutput(x: Exp[Any]) = emitConstOrSym(x, "output")

  def emitEOG() = {
    stream.print("{\"type\":\"EOG\"}\n],\n")
  }

  def emitSubGraph(prefix: String, e: Block[Any]) = e match {
    case Block(c:Const[Any]) => stream.println("  \"" + prefix + "Type\": \"const\",")
                                stream.println("  \"" + prefix + "Value\": \"" + quote(c) + "\",")
    case Block(s:Sym[Any]) =>   getBlockResult(e) match {
                                  // if we have a non-unit constant return value, we need to be able to parse it directly
                                  case c: Const[Any] if c.tp != manifest[Unit] =>
                                    stream.println("  \"" + prefix + "Type\": \"const\",")
                                    stream.println("  \"" + prefix + "Value\": \"" + quote(c) + "\",")
                                  case _ =>
                                    stream.println("  \"" + prefix + "Type\": \"symbol\",")
                                }
                                stream.println("  \"" + prefix + "Ops\": [")
                                val saveMutatingDeps = kernelMutatingDeps
                                val saveEffectKernelReads = effectKernelReads
                                kernelMutatingDeps = Map()
                                effectKernelReads = Map()
                                emitBlock(e)
                                emitEOG()
                                effectKernelReads = saveEffectKernelReads
                                kernelMutatingDeps = saveMutatingDeps
  }

  private def makeString(list: List[Exp[Any]]) = {
    if(list.isEmpty) "" else list.map(quote(_)).mkString("\"","\",\"","\"")
  }

  private def getOutputTypes(sym: Exp[Any]) = {
    (for (gen <- generators) yield {
      try {
        Some("\"" + gen.toString + "\" : \"" + gen.remap(sym.tp)  + "\"")
      } catch {
        case e:GenerationFailedException => None
        case e:Exception => throw(e)
      }
    }).filter(_.isDefined).map(_.get)
  }

  override def emitBlockHeader(args: List[Sym[Any]], appName: String) {
    stream.println("{\"DEG\":{\n"+
                   "\"version\" : 0.1,\n"+
                   "\"name\" : \"" + appName + "\",\n"+
                   "\"kernelpath\" : \"" + Config.buildDir  + "\",\n"+
                   "\"targets\": [" + generators.map("\""+_+"\"").mkString(",")  + "],\n"+
                   "\"ops\": [")

    // CLikeCodegen types need to know all the types used in the program including args
    // because even array types are generated by the compiler rather using template.
    def nestedManifests(m: Manifest[_]): List[Manifest[_]] = {
      if (m.typeArguments.isEmpty) List(m)
      else (List(m) ++ m.typeArguments.flatMap(nestedManifests)).distinct
    }
    for (arg <- args) {
      for (gen <- generators) {
        gen match {
          case g: CLikeCodegen => nestedManifests(arg.tp) foreach { m =>
            try {
              g.dsTypesList.add((m,g.remap(m)))
            }
            catch {
              case e: GenerationFailedException => None
              case e: Exception => throw(e)
            }
          }
          case _ => //
        }
      }
    }

    for (i <- 0 until args.length) {
      stream.println("{\"type\" : \"Arguments\", \"kernelId\" : \"" + quote(args(i)) + "\", \"index\" : \"" + i + "\",")
      stream.println("  \"return-types\":{" + getOutputTypes(args(i)).mkString(",") + "}")
      stream.println("},")
    }
  }

  override def emitBlockFooter(result: Exp[Any]) {
    stream.println("{\"type\":\"EOP\",")
    stream.println("  \"return-types\":{" + getOutputTypes(result).mkString(",") + "},")
    emitConstOrSym(result, "")
    stream.println("}\n]}}")
  }

}
