package ppl.delite.framework.codegen.delite

import generators.{DeliteGenTaskGraph}
import overrides.{DeliteScalaGenVariables, DeliteCudaGenVariables, DeliteAllOverridesExp}
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common.{BaseGenStaticData, StaticDataExp}
import ppl.delite.framework.{Config, DeliteApplication}
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}
import ppl.delite.framework.extern.DeliteGenExternal
import scala.reflect.SourceContext

/**
 * Notice that this is using Effects by default, also we are mixing in the Delite task graph code generator
 */
trait DeliteCodegen extends GenericFatCodegen with BaseGenStaticData with ppl.delite.framework.codegen.Utils {
  val IR: Expressions with FatExpressions with Effects with StaticDataExp
  import IR._

  // these are the target-specific kernel generators (e.g. scala, cuda, etc.)
  type Generator = GenericFatCodegen{val IR: DeliteCodegen.this.IR.type}
  val generators : List[Generator]

  // per kernel, used by DeliteGenTaskGraph
  var controlDeps : List[Sym[Any]] = _
  var emittedNodes : List[Sym[Any]] = _

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
  override def fatten(e: TP[Any]): TTP = ifGenAgree(_.fatten(e))

  // fusion stuff...
  override def unapplySimpleIndex(e: Def[Any]) = ifGenAgree(_.unapplySimpleIndex(e))
  override def unapplySimpleCollect(e: Def[Any]) = ifGenAgree(_.unapplySimpleCollect(e))

  override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]) = ifGenAgree(_.shouldApplyFusion(currentScope)(result))

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
  
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any],Any)] = {

    val x = fresh[A]
    val y = reifyEffects(f(x))

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
    emitBlock(y)(stream)
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
  override def emitFatBlockFocused(currentScope: List[TTP])(result: List[Block[Any]])(implicit stream: PrintWriter): Unit = {
    printlog("-- block for "+result)
    currentScope.foreach(printlog(_))

/*
    println("-- shallow schedule for "+result)
    shallow = true
    val e2 = getFatSchedule(currentScope)(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false
    e2.foreach(println)
    println("-- bound for "+result)
    val e1 = currentScope
    val bound = e1.flatMap(z => boundSyms(z.rhs))
    bound.foreach(println)
    
    println("-- dependent for "+result)
    bound.foreach { st =>
      val res = getFatDependentStuff0(currentScope)(st)
      println("--- dep on " + st)
      res.foreach(println)
    }
*/
    focusExactScopeFat(currentScope)(result) { levelScope => 
      printlog("-- level for "+result)
      levelScope.foreach(printlog(_))
      printlog("-- exact for "+result)
      availableDefs.foreach(printlog(_))

/*
      val effects = result match {
        case Def(Reify(x, effects0)) =>
          println("*** effects0: " + effects0)
          
          levelScope.filter(fb => fb.lhs.exists(effects0 contains _)) // all e whose lhs contains an effect
        case _ => Nil
      }

      //println("*** effects1: " + effects.flatMap(_.lhs))
      //val effectsN = levelScope.collect { case TTP(List(s), ThinDef(Reflect(_, es))) => s } // TODO: Mutation!!
      //println("*** effectsN: " + effectsN)
      
      // TODO: do we need to override this method? effectsN can be taken from
      // the reflect nodes during emitFatNode in DeliteGenTaskGraph
*/      
      
      val localEmittedNodes = new ListBuffer[Sym[Any]]
      val controlNodes = new ListBuffer[Sym[Any]]

      controlDeps = Nil

      for (TTP(syms, rhs) <- levelScope) {
        // we only care about effects that are scheduled to be generated before us, i.e.
        // if e4: (n1, n2, e1, e2, n3), at n1 and n2 we want controlDeps to be Nil, but at
        // n3 we want controlDeps to contain e1 and e2
        //controlDeps = levelScope.takeWhile(_.lhs != syms) filter { effects contains _ } flatMap { _.lhs }
        //controlDeps = Nil // within emitFatNode below iff it is a reflect/reify node <-- wrong code in runtime
        rhs match {
          // TODO: fat loops with embedded reflects??
          case ThinDef(Reflect(_,_,_)) => controlNodes ++= syms
          case ThinDef(Reify(_,_,_)) =>
          case _ => localEmittedNodes ++= syms
        }
        emitFatNode(syms, rhs)
        controlDeps = controlNodes.toList // need to do it that way... TODO: set only if changed
      }
      
      emittedNodes = localEmittedNodes.result // = levelScope.flatMap(_.syms) ??
    }
  }



 /**
  * Return a list of all effectful operations rooted at start.
  */
  def getEffectsBlock(start: Def[Any]): List[Sym[Any]] = {
    //val g = generators(0) // skip ifGenAgree for now...

    // val deps = g.blocks(start) // can optimize by adding a syms-like function that only returns blocks (but more invasive)
//    val deps = g.syms(start)
//    val nodes = deps flatMap { b =>
//      g.focusBlock(b) {
//        g.focusExactScope(b) { _.flatMap { e =>
//          val eff = e.sym match {
//            case Def(Reflect(x, u, effects)) => List(e.sym): List[Sym[Any]]
//            case _ => Nil
//          }
//          eff ::: getEffectsBlock(e.rhs)
//        }}
//      }
//    }
    val nodes = boundSyms(start) filter { case Def(Reflect(x, u, effects)) => true; case _ => false }
    nodes.distinct
  }


  def getEffectsBlock(defs: List[Def[Any]]): List[Sym[Any]] = {
    defs flatMap { getEffectsBlock(_) } distinct
  }


  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

}

trait DeliteCodeGenPkg extends DeliteGenTaskGraph with DeliteGenExternal
