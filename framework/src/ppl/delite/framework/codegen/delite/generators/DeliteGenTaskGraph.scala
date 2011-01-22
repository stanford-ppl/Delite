package ppl.delite.framework.codegen.delite.generators

import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.{Util, Config}
import collection.mutable.{ArrayBuffer, ListBuffer, HashMap}
import java.io.{StringWriter, FileWriter, File, PrintWriter}
import scala.virtualization.lms.common.LoopFusionOpt
import scala.virtualization.lms.internal.{GenerationFailedException}

trait DeliteGenTaskGraph extends DeliteCodegen with LoopFusionOpt {
  val IR: DeliteOpsExp
  import IR._

  private def vals(sym: Sym[Any]) : List[Sym[Any]] = sym match {
    case Def(Reify(s, effects)) => List(s.asInstanceOf[Sym[Any]])
    case Def(Reflect(NewVar(v), effects)) => Nil
    case _ => List(sym)
  }

  private def vars(sym: Sym[Any]) : List[Sym[Any]] = sym match {
    case Def(Reflect(NewVar(v), effects)) => List(sym)
    case _ => Nil
  }

  // FIXME !!! this is probably not accurate
  private def mutating(kernelContext: State, sym: Sym[Any]) : List[Sym[Any]] =
    kernelContext flatMap {
      //case Def(Reflect(x,effects)) => if (syms(x) contains sym) List(sym) else Nil
      case Def(Mutation(x,effects)) => if (syms(x) contains sym) List(sym) else Nil
      case _ => Nil
    }


  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match { // getFreeVarBlock(syms(rhs), boundSyms(rhs))
    case Reflect(s, effects) => getFreeVarNode(s)
    case _ => super.getFreeVarNode(rhs)
  }


  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = {
    assert(generators.length >= 1)

    println("DeliteGenTaskGraph.emitNode "+sym+"="+rhs)

    val kernelName = sym.map(quote).mkString("")

    var resultIsVar = false
    var skipEmission = false
    //nestedEmission = false TODO ?
    var nestedNode: TP[Any] = null
    implicit var emittedNodeList = new ListBuffer[List[Sym[Any]]]

    val saveInputDeps = kernelInputDeps
    val saveMutatingDeps = kernelMutatingDeps

    // we will try to generate any node that is not purely an effect node
    rhs match {
      case ThinDef(Reflect(s, effects)) =>
        controlDeps = effects; // <---  now handling control deps here...!!
        super.emitFatNode(sym, rhs); return
      case ThinDef(Reify(s, effects)) =>
        super.emitFatNode(sym, rhs); return
      case ThinDef(DeliteOpCondition(c,t,e)) => {
        emitBlock(c)
        emittedNodeList += controlDeps
        emitBlock(t)
        emittedNodeList += emittedNodes
        emitBlock(e)
        emittedNodeList += emittedNodes
        skipEmission = true
      }
      case ThinDef(DeliteOpIndexedLoop(s,e,i,b)) => {
        val saveMutatingDeps = kernelMutatingDeps
        val saveInputDeps = kernelInputDeps
        emitBlock(b)
        emittedNodeList += emittedNodes
        skipEmission = true
      }
      case ThinDef(DeliteOpWhileLoop(c,b)) => {
        emitBlock(c)
        emittedNodeList += controlDeps
        emittedNodeList += emittedNodes
        emitBlock(b)
        emittedNodeList += emittedNodes
        skipEmission = true
      }
      case ThinDef(NewVar(x)) => resultIsVar = true // if sym is a NewVar, we must mangle the result type
      case _ => // continue and attempt to generate kernel
    }

    kernelInputDeps = saveInputDeps
    kernelMutatingDeps = saveMutatingDeps

    // validate that generators agree on inputs (similar to schedule validation in DeliteCodegen)
    //val dataDeps = ifGenAgree(g => (g.syms(rhs) ++ g.getFreeVarNode(rhs)).distinct, true)
    
    val dataDeps = // don't use getFreeVarNode...
      focusFatBlock(syms(rhs)) { freeInScope(boundSyms(rhs), syms(rhs)) } distinct
      //syms(rhs).flatMap(s => focusBlock(s) { freeInScope(boundSyms(rhs), s) } ).distinct
    

    val inVals = dataDeps flatMap { vals(_) }
    val inVars = dataDeps flatMap { vars(_) }

    implicit val supportedTargets = new ListBuffer[String]
    implicit val returnTypes = new ListBuffer[Pair[String, String]]
    implicit val outputSlotTypes = new HashMap[String, ListBuffer[(String, String)]]
    implicit val metadata = new ArrayBuffer[Pair[String, String]]
    
    // parameters for delite overrides
    deliteInputs = (inVals ++ inVars)
    deliteResult = Some(sym) //findDefinition(rhs) map { _.sym }

    for (gen <- generators) {
      // reset nested flag
      //gen.nestedEmission = false TODO ?
      val buildPath = Config.buildDir + java.io.File.separator + gen + java.io.File.separator
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
        if (!skipEmission) {
          gen.emitKernelHeader(sym, inVals, inVars, resultType, resultIsVar)(kstream)
          kstream.println(bodyString.toString)
          gen.emitKernelFooter(sym, inVals, inVars, resultType, resultIsVar)(kstream)
        } else {
          kstream.println("// skipped emission")
        }

        // record that this kernel was successfully generated
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
        if (gen.hasMetaData) {
          metadata += new Pair[String,String](gen.toString, gen.getMetaData) {
            override def toString = "\"" + _1 + "\" : " + _2
          }
        }

        kstream.close()
      }
      catch {
        case e:GenerationFailedException => // no generator found
          gen.exceptionHandler(e, outFile, kstream)
          //println(quote(sym))
          //e.printStackTrace
        case e:Exception => throw(e)
      }
    }

    if (skipEmission == false && supportedTargets.isEmpty) {
      var msg = "Node " + quote(sym) + "[" + rhs + "] could not be generated by any code generator"
      // TODO? if(nestedEmission) msg = "Failure is in nested node " + quote(nestedNode.sym) + "[" + nestedNode.rhs + "]. " + msg
      system.error(msg)
    }

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
        emitMultiLoop(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps, op.body.exists(_.isInstanceOf[DeliteReduceElem[_]]))
      case ThinDef(z) => z match {
        case DeliteOpCondition(c,t,e) => emitIfThenElse(c,kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case DeliteOpIndexedLoop(s,e,i,b) => emitIndexedLoop(s,e,i, kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case DeliteOpWhileLoop(c,b) => emitWhileLoop(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case s:DeliteOpSingleTask[_] => emitSingleTask(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case op:AbstractLoop[_] => emitMultiLoop(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps, op.body.isInstanceOf[DeliteReduceElem[_]])
        case m:DeliteOpMap[_,_,_] => emitMap(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case r:DeliteOpReduce[_] => emitReduce(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case a:DeliteOpMapReduce[_,_,_] => emitMapReduce(kernelName, outputs, inputs, inMutating,inControlDeps, antiDeps)
        case z:DeliteOpZipWith[_,_,_,_] => emitZipWith(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        // ZipWithReduce??
        case f:DeliteOpForeach[_,_] => emitForeach(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps)
        case _ => emitSingleTask(kernelName, outputs, inputs, inMutating, inControlDeps, antiDeps) // things that are not specified as DeliteOPs, emit as SingleTask nodes
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

  def emitSingleTask(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"SingleTask\"")
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }

  def emitMultiLoop(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], needsCombine: Boolean)
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"MultiLoop\", \"needsCombine\":" + needsCombine)
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }
  
  def emitMap(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"Map\"")
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }

  def emitReduce(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"Reduce\"")
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }

  def emitMapReduce(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"MapReduce\"")
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }

  def emitZipWith(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"ZipWith\"")
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }

  def emitForeach(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"Foreach\"")
    emitExecutionOpCommon(id, outputs, inputs, mutableInputs, controlDeps, antiDeps)
  }

  def emitIfThenElse(cond: Exp[Boolean], id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print("{\"type\":\"Conditional\",")
    stream.println("  \"outputId\" : \"" + id + "\",")
    stream.print("  \"outputs\":[" + outputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    val bodyIds = emittedNodesList(1) ++ emittedNodesList(2)
    val controlDepsStr = makeString(controlDeps filterNot { bodyIds contains })
    val antiDepsStr = makeString(antiDeps filterNot { bodyIds contains })
    val thenS = makeString(emittedNodesList(1))
    val elseS = makeString(emittedNodesList(2))
    stream.println("  \"conditionKernelId\" : \"" + quote(cond) + "\", ")
    stream.println("  \"thenKernelIds\" : [" + thenS + "],")
    stream.println("  \"elseKernelIds\" : [" + elseS + "],")
    stream.print("  \"controlDeps\":[" + controlDepsStr + "],\n")
    stream.println("  \"antiDeps\":[" + antiDepsStr + "]")
    stream.println("},")
  }

  def emitIndexedLoop(start: Exp[Int], end: Exp[Int], i: Exp[Int], id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.println("{\"type\":\"IndexedLoop\",")
    stream.println("  \"outputId\" : \"" + id + "\",")
    val controlDepsStr = makeString(controlDeps filterNot { emittedNodesList(0) contains })
    val antiDepsStr = makeString(antiDeps filterNot { emittedNodesList(0) contains })
    def getType(e: Exp[Int]) = e match {
      case c:Const[Int] => "const"
      case s:Sym[Int]   => "symbol"
    }
    stream.print("  \"startType\" : \"" + getType(start) + "\",")
    stream.println(" \"startValue\" : \"" + quote(start) + "\",")
    stream.print("  \"endType\" : \"" + getType(end) + "\",")
    stream.println(" \"endValue\" : \"" + quote(end) + "\",")
    stream.println("  \"indexId\" : \"" + quote(i) + "\",")
    val bodyS = makeString(emittedNodesList(0))
    stream.println("  \"bodyIds\" : [" + bodyS + "],")
    stream.print("  \"controlDeps\":[" + controlDepsStr + "],\n")
    stream.println("  \"antiDeps\":[" + antiDepsStr + "]")
    stream.println("},")
  }

  def emitWhileLoop(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.println("{\"type\":\"WhileLoop\",")
    stream.println("  \"outputId\" : \"" + id + "\",")
    val controlDepsStr = makeString(controlDeps filterNot { emittedNodesList(2) contains })
    val antiDepsStr = makeString(antiDeps filterNot { emittedNodesList(2) contains })
    val conds = makeString(emittedNodesList(1))
    val bodys = makeString(emittedNodesList(2))
    stream.println("  \"condIds\" : [" + conds + "],")
    stream.println("  \"bodyIds\" : [" + bodys + "],")
    stream.print("  \"controlDeps\":[" + controlDepsStr + "],\n")
    stream.println("  \"antiDeps\":[" + antiDepsStr + "]")
    stream.println("},")
  }

  def emitControlFlowOpCommon(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
  }

  def emitExecutionOpCommon(id: String, outputs: List[Exp[Any]], inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit stream: PrintWriter, supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]], emittedNodesList: ListBuffer[List[Sym[Any]]]) = {
    stream.print(" , \"kernelId\" : \"" + id + "\" ")
    stream.print(" , \"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "],\n")
    stream.print("  \"outputs\":[" + outputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    stream.print("  \"inputs\":[" + inputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    stream.print("  \"mutableInputs\":[" + mutableInputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    emitDepsCommon(controlDeps, antiDeps)
    stream.print("  \"metadata\":{" + metadata.mkString(",") + "},\n")
    stream.print("  \"return-types\":{" + returnTypes.mkString(",") + "}")

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

  def emitDepsCommon(controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], last:Boolean = false)(implicit stream: PrintWriter) {
    stream.print("  \"controlDeps\":[" + makeString(controlDeps) + "],\n")
    stream.print("  \"antiDeps\":[" + makeString(antiDeps) + "]" + (if(last) "\n" else ",\n"))
  }

  private def makeString(list: List[Exp[Any]]) = {
    if(list.isEmpty) "" else list.map(quote(_)).mkString("\"","\",\"","\"")
  }

/*
  // more quirks
  override def quote(x: Exp[Any]) = x match {
    case r:Reify[_] => quote(r.x) //DISCUSS <- what's the purpose of this? it will never match because Reify is a Def, not Exp
    case _ => super.quote(x)
  }
*/



  def nop = throw new RuntimeException("Not Implemented Yet")

}
