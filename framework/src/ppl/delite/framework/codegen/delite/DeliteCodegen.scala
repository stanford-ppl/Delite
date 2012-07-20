package ppl.delite.framework.codegen.delite

import java.io.{FileWriter, BufferedWriter, File, PrintWriter}
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import generators.{DeliteGenTaskGraph}
import overrides.{DeliteScalaGenVariables, DeliteCudaGenVariables, DeliteAllOverridesExp}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.transform.ForwardPassTransformer
import ppl.delite.framework.ops.DeliteOpsExp

/**
 * Notice that this is using Effects by default, also we are mixing in the Delite task graph code generator
 */
trait DeliteCodegen extends GenericFatCodegen with BaseGenStaticData with ppl.delite.framework.codegen.Utils {
  val IR: DeliteOpsExp //Expressions with FatExpressions with Effects with StaticDataExp with LoopsFatExp with IfThenElseFatExp
  import IR._

  // these are the target-specific kernel generators (e.g. scala, cuda, etc.)
  type Generator = GenericFatCodegen{val IR: DeliteCodegen.this.IR.type}
  val generators: List[Generator]

  // should be set by DeliteApplication if there are any transformations to be run before codegen
  var transformers: List[WorklistTransformer{val IR: DeliteCodegen.this.IR.type}] = Nil
  
  // per kernel, used by DeliteGenTaskGraph
  var controlDeps: List[Sym[Any]] = _
  var emittedNodes: List[Sym[Any]] = _

  // global, used by DeliteGenTaskGraph
  var kernelMutatingDeps = Map[Sym[Any],List[Sym[Any]]]() // from kernel to its mutating deps
  var kernelInputDeps = Map[Sym[Any],List[Sym[Any]]]() // from kernel to its input deps


  def ifGenAgree[A](f: Generator => A): A = {
    //val save = generators map { _.shallow }
    //generators foreach { _.shallow = shallow }
    val result = generators map f
    if (result.distinct.length != 1){
      sys.error("DeliteCodegen: generators disagree")
    }
    //for (i <- 0 until generators.length) {
    //  generators(i).shallow = save(i)
    //}
    result(0)
  }

  override def emitDataStructures(path: String): Unit = {
    val s = File.separator
    for (g <- generators) {
      val dsRoot = Config.homeDir + s+"framework"+s+"src"+s+"ppl"+s+"delite"+s+"framework"+s+"datastruct"+s+g
      copyDataStructures(dsRoot, path+s+g+s+"datastructures")
    }
  }

  // TODO: move to some other place? --> get rid of duplicate in embedded generators!
  override def fatten(e: Stm): Stm = ifGenAgree(_.fatten(e))

  // fusion stuff...
  override def unapplySimpleIndex(e: Def[Any]) = ifGenAgree(_.unapplySimpleIndex(e))
  override def unapplySimpleCollect(e: Def[Any]) = ifGenAgree(_.unapplySimpleCollect(e))

  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]) = ifGenAgree(_.shouldApplyFusion(currentScope)(result))

  def emitSourceContext(sourceContext: Option[SourceContext], stream: PrintWriter, id: String) {
    // obtain root parent source context (if any)
    val parentContext: Option[SourceContext] =
      if (!sourceContext.isEmpty) {
        var current = sourceContext.get
        while (!current.parent.isEmpty)
          current = current.parent.get
        Some(current)
      } else
    	  sourceContext
    
    stream.print("  \"sourceContext\": {\n    ")
    val (fileName, line, opName) =
      if (parentContext.isEmpty) ("<unknown file>", 0, id) else {
        val sc = parentContext.get
        (sc.fileName, sc.line, sc.methodName)
      }
    stream.print("\"fileName\": \"" + fileName + "\",\n    ")
    stream.print("\"opName\": \"" + opName + "\",\n    ")
    stream.print("\"line\": \"" + line + "\" }")
  }
  
/*
{"SymbolMap": [
  {"symbol": "x8", "sourceContext": {
    "fileName": "/Users/phaller/git/Delite-rw/apps/scala/src/ppl/apps/ml/gda/GDA.scala",
    "opName": "length",
    "line": "16" }
  }
  {"symbol": "x9", "sourceContext": {
    "fileName": "/Users/phaller/git/Delite-rw/apps/scala/src/ppl/apps/ml/gda/GDA.scala",
    "opName": "count",
    "line": "18" }
  }
] }
 */
  def emitSymbolSourceContext(stream: PrintWriter): Unit = {
    // output header
    stream.println("{\"SymbolMap\": [")
    // output map from symbols to SourceContexts
    var first = true
    for (TP(sym, _) <- globalDefs) {
      if (first) { first = false }
      else stream.print(", ")
      stream.print("{\"symbol\": \"x" + sym.id + "\",")
      emitSourceContext(if (sym.sourceContexts.isEmpty) None else Some(sym.sourceContexts.head), stream, "x"+sym.id)
      stream.println("}")
    }
    stream.println("] }")
  }
  
  def runTransformations[A:Manifest](b: Block[A]): Block[A] = {
    printlog("DeliteCodegen: applying transformations")
    var curBlock = b  
    printlog("  Transformers: " + transformers)    
    val maxTransformIter = 3 // TODO: make configurable
    for (t <- transformers) {
      printlog("  Block before transformation: " + curBlock)    
      printlog("  map: " + t.nextSubst)
      var i = 0
      while (!t.isDone && i < maxTransformIter) {
        printlog("iter: " + i)
        curBlock = t.runOnce(curBlock)
        i += 1
      }
      if (i == maxTransformIter) printlog("  warning: transformer " + t + " did not converge in " + maxTransformIter + " iterations")
      printlog("  Block after transformation: " + curBlock) 
    }
    printlog("DeliteCodegen: done transforming")    
    curBlock   
  }
  
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any],Any)] = {

    val x = fresh[A]
    val b = reifyEffects(f(x)) // transformers only get registrations at this point, while the IR is being constructed    
    val y = runTransformations(b)
    
    val sA = mA.toString
    val sB = mB.toString

    val staticData = getFreeDataBlock(y)

    printlog("-- emitSource")
    availableDefs.foreach(printlog(_))
    
    stream.println("{\"DEG\":{\n"+
                   "\"version\" : 0.1,\n"+
                   "\"kernelpath\" : \"" + Config.buildDir  + "\",\n"+
                   "\"targets\": [" + generators.map("\""+_+"\"").mkString(",")  + "],\n"+
                   "\"ops\": [")

    stream.println("{\"type\" : \"Arguments\" , \"kernelId\" : \"x0\"},")
    withStream(stream)(emitBlock(y))
    //stream.println(quote(getBlockResult(y)))
    stream.println("{\"type\":\"EOP\"}\n]}}")

    if (Config.enableProfiler) {
      val symbolsFilename =
        Config.degFilename.substring(0, Config.degFilename.length() - 4) + "-symbols.json"
      val writer = new FileWriter(symbolsFilename)
      val printer = new PrintWriter(writer)
      emitSymbolSourceContext(printer)
      printer.flush
      writer.flush
    }
    
    stream.flush
    staticData
  }


  /**
   * DeliteCodegen expects there to be a single schedule across all generators, so a single task graph
   * can be generated. This implies that every generator object must compute internal dependencies (syms)
   * the same way.
   *
   * This is all because we allow individual generators to refine their dependencies, which directly impacts
   * the generated schedule. We may want to consider another organization.
   */
  override def traverseBlockFocused[A](block: Block[A]): Unit = {
    printlog("-- block for " + block)
    innerScope.foreach(printlog(_))

    focusExactScope(block) { levelScope =>
      printlog("-- level for " + block)
      levelScope.foreach(printlog(_))
      printlog("-- exact for " + block)
      availableDefs.foreach(printlog(_))

      val localEmittedNodes = new ListBuffer[Sym[Any]]
      val controlNodes = new ListBuffer[Sym[Any]]

      controlDeps = Nil

      for (stm <- levelScope) {
        // we only care about effects that are scheduled to be generated before us, i.e.
        // if e4: (n1, n2, e1, e2, n3), at n1 and n2 we want controlDeps to be Nil, but at
        // n3 we want controlDeps to contain e1 and e2
        //controlDeps = levelScope.takeWhile(_.lhs != syms) filter { effects contains _ } flatMap { _.lhs }
        //controlDeps = Nil // within emitFatNode below iff it is a reflect/reify node <-- wrong code in runtime
        val syms = stm.lhs
        val rhs = stm.rhs

        rhs match {
          // TODO: fat loops with embedded reflects??
          case Reflect(_,_,_) => controlNodes ++= syms
          case Reify(_,_,_) =>
          case _ => localEmittedNodes ++= syms
        }

        def emitAnyNode(syms: List[Sym[Any]], rhs: Any) = rhs match { //should this be part of the API or always hidden (with only emitNode and emitFatNode public)
          case d: Def[_] => 
            assert(syms.length == 1)
            emitNode(syms(0), d)
          case fd: FatDef =>
            emitFatNode(syms, fd)
        }

        emitAnyNode(syms,rhs)
        controlDeps = controlNodes.toList // need to do it that way... TODO: set only if changed
      }

      emittedNodes = localEmittedNodes.result // = levelScope.flatMap(_.syms) ??
    }
  }


 /**
  * Return a list of all effectful operations rooted at start.
  */  
  def getEffectsBlock(start: Def[Any]): List[Sym[Any]] = {
    val nodes = boundSyms(start) filter { case Def(Reflect(x, u, effects)) => true; case _ => false }
    nodes.distinct
  }

  def getEffectsBlock(defs: List[Def[Any]]): List[Sym[Any]] = {
    defs flatMap { getEffectsBlock(_) } distinct
  }


  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String): Unit = {
    stream.println(lhs + " = " + rhs)
  }

}

trait DeliteCodeGenPkg extends DeliteGenTaskGraph
