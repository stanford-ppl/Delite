package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.{HashMap,HashSet,ArrayBuffer}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{Expressions,AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext
import scala.io.Source

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayOpsExp,DeliteArrayFatExp,BaseGenDeliteArrayOps}
import ppl.delite.framework.Config
import ppl.delite.framework.transform.LoopSoAOpt
import ppl.delite.framework.ops.BaseDeliteOpsTraversalFat

//TODO:
// 1. Unroll the loop if some operations in the loop body cannot be generated for GPU (e.g., OP_Extern)

trait NestedLoopMappingExp extends Expressions {

  case class LoopConstraint(d: Dimension => Boolean, b: Int => Boolean, s: Span => Boolean, w: Double = 1.0)

  def infix_and(c1: LoopConstraint, c2: LoopConstraint) = LoopConstraint(c => c1.d(c) && c2.d(c),
                                                                         c => c1.b(c) && c2.b(c),
                                                                         c => c1.s(c) && c2.s(c))
  def infix_or(c1: LoopConstraint, c2: LoopConstraint) = LoopConstraint(c => c1.d(c) || c2.d(c),
                                                                        c => c1.b(c) || c2.b(c),
                                                                        c => c1.s(c) || c2.s(c))

  def infix_withWeight(c: LoopConstraint, w: Double) = LoopConstraint(c.d, c.b, c.s, w)

  // Dimension Constraints
  abstract class Dimension
  case object DimX extends Dimension { override def toString = "\"dim\":\"x\"" }
  case object DimY extends Dimension { override def toString = "\"dim\":\"y\"" }
  case object DimZ extends Dimension { override def toString = "\"dim\":\"z\"" }
  def dimConstraint(c: Dimension => Boolean) = LoopConstraint(c, _ => true, _ => true)
  
  // Span Constraint
  abstract class Span { def toString(quote: Exp[Any] => String): String = toString() }
  case object SpanOne extends Span
  case class SpanOne(s: Exp[Int]) extends Span { override def toString(quote: Exp[Any] => String) = "\"span\":{\"tpe\":\"one\",\"size\":\""+quote(s)+"\"}" }
  case class SpanN(s: Exp[Int], n: Int) extends Span { override def toString(quote: Exp[Any] => String) = "\"span\":{\"tpe\":\"n\",\"size\":\""+quote(s)+"\",\"factor\":\""+n+"\"}" }
  case class Split(s: Exp[Int], k: Int) extends Span { override def toString(quote: Exp[Any] => String) = "\"span\":{\"tpe\":\"split\",\"size\":\""+quote(s)+"\",\"factor\":\""+k+"\"}" }
  case object SpanAll extends Span { override def toString(quote: Exp[Any] => String) = "\"span\":{\"tpe\":\"all\",\"size\":\"Any\"}" }
  def spanConstraint(c: Span => Boolean) = LoopConstraint(_ => true, _ => true, c)

  // Block Size Constraint
  abstract class BlockSize
  def blockSizeConstraint(c: Int => Boolean) = LoopConstraint(_ => true, c, _ => true)

  // result of the analysis
  val loopAnalysisResult = new HashMap[Int, (Dimension,Int,Span)]
}

trait NestedLoopMappingAnalysis extends FatBlockTraversal with LoopFusionOpt with LoopSoAOpt with BaseGenDeliteArrayOps with BaseDeliteOpsTraversalFat {
  val IR: DeliteOpsExp
  import IR._

  class MultiDimMappingFailedException(val msg: String) extends Exception(msg)

  var kernelInputs: List[Sym[Any]] = null

  val softConstraints = new HashMap[LoopInfo, HashSet[LoopConstraint]]()
  val hardConstraints = new HashMap[Int, HashSet[LoopConstraint]]()

  private def addHardC(hcIdx: Int, to: LoopInfo): Unit = {
    val c = hcIdx match {
      case 1 => spanConstraint(_ == SpanAll)
      case _ => throw new RuntimeException("Unknown hard constraint idx:" + hcIdx)
    }
    hardConstraints.getOrElseUpdate(to.level, HashSet[LoopConstraint]()).add(c)
  }

  // intrinsic weights for soft constraints
  private val iWeights = Map[Int,Double](1->10.0, 2->5.0, 3->2.0)

  private def addSoftC(scIdx: Int, to: LoopInfo, at: LoopInfo): Unit = {
    val c = scIdx match {
      case 1 =>
        //TODO: if the size is less than 32, don't add size constraint
        dimConstraint(_ == DimX) and blockSizeConstraint(_ % 32 == 0)
      case 2 =>
        spanConstraint(_ == SpanOne)
      case _ => throw new RuntimeException("Unknown soft constraint idx: " + scIdx)
    }
    val weight = nestedSize(at) match {
      case Some(s) => s * iWeights(scIdx)
      case None => throw new RuntimeException("Cannot find the nested size of loop :" + at)
    }
    softConstraints.getOrElseUpdate(to, HashSet[LoopConstraint]()).add(c withWeight(weight))
  }

  def verbose = Config.debug
  def printInfo(x: String) = if (verbose) Predef.println("[Loop Analysis] " + x) else ()

  private def distinctAccess(body: Def[Any]): Boolean = {
    body match {
      case elem: DeliteCollectElem[_,_,_] if (elem.cond == Nil) => true
      case elem: DeliteForeachElem[_] => true
      case _ => false
    }
  }

  private def infix_size(m: LoopInfo): Exp[Int] = {
    m.loop match {
      case SimpleFatLoop(sz,_,_) => sz
      case SimpleLoop(sz,_,_) => sz
      case Reflect(l:AbstractLoop[_],u,es) => l.size
      case l:AbstractLoop[_] => l.size
      case _ => throw new RuntimeException("Cannot match loop for size")
    }
  }

  private def infix_index(m: LoopInfo): Exp[Int] = {
    m.loop match {
      case SimpleFatLoop(_,v,_) => v
      case SimpleLoop(_,v,_) => v
      case Reflect(l:AbstractLoop[_],u,es) => l.v
      case l:AbstractLoop[_] => l.v
      case _ => throw new RuntimeException("Cannot match loop for index")
    }
  }

  private def infix_elems(m: LoopInfo): List[Def[Any]] = {
    m.loop match {
      case SimpleFatLoop(_,_,body) => body
      case SimpleLoop(_,_,body) => List(body)
      case Reflect(l:AbstractLoop[_],u,es) => List(l.body)
      case l:AbstractLoop[_] => List(l.body)
      case _ => throw new RuntimeException("Cannot match loop for elems")
    }
  }

  private def infix_bodies(m: LoopInfo): List[Block[Any]] = {
    m.loop match {
      case SimpleFatLoop(_,_,body) => collectLoopBodyFat(body)
      case SimpleLoop(_,_,body) => collectLoopBody(body)
      case Reflect(l:AbstractLoop[_],u,es) => collectLoopBody(l.body)
      case l:AbstractLoop[_] => collectLoopBody(l.body)
      case _ => throw new RuntimeException("Cannot match loop for body")
    }
  }

  case class LoopInfo(sym: List[Sym[Any]], loop: Any, level: Int, nest: ArrayBuffer[LoopInfo])

  var currentLevel: Int = 0
  var currentLoop: LoopInfo = null
  val loopInfoRoot: LoopInfo = null

  private def allLoops(st: LoopInfo): List[LoopInfo] = {
    List(st) ++ st.nest.flatMap(allLoops(_)).toList
  }

  private def sizeSymsAtLevel(level: Int): List[Exp[Int]] = {
    allLoops(loopInfoRoot).filter(_.level == level).map(_.size).distinct
  }

  // default value for loop size when unkown at compile time
  val defaultLoopSize = 1000

  private def sizeAtLevel(level: Int): Int = {
    allLoops(loopInfoRoot).filter(_.level == level).map(_.size).map(_ match {
      case Const(c) => c
      case _ => defaultLoopSize
    }).max
  }

  private def nestedSize(m: LoopInfo, head: LoopInfo = loopInfoRoot): Option[Int] = {
    def loopSize(l: LoopInfo): Int = {
      l.size match {
        case Const(c) => c
        case _ => defaultLoopSize
      }
    }
    head.loop match {
      case m.loop => Some(loopSize(m))
      case _ => head.nest.find(n => nestedSize(m,n).nonEmpty).map(loopSize)
    }
  }

  def analyzeLoopBodies(m: LoopInfo): Unit = {
    val saveLevel = currentLevel
    val saveLoop = currentLoop
    currentLevel = currentLevel + 1
    currentLoop = m

    traverseFatBlock(m.bodies)

    // add constraint on the loop access pattern
    m.elems.find(!distinctAccess(_)) match {
      case Some(_) =>
        //TODO: enable this
        if (currentLevel < 2) throw new MultiDimMappingFailedException("outer-most level combine is not implemented yet at runtime codegen")
        else addHardC(1, m)
      case None =>
        addSoftC(2, m, m)
    }

    currentLevel = saveLevel
    currentLoop = saveLoop
  }

  object LoopIndex {
    def unapply(i: Exp[Int]): Option[LoopInfo] = {
      allLoops(loopInfoRoot).find(_.index == i)
    }
  }

  private def processArrayAccess(i: Exp[Int]) {
    printInfo("processing array access " + quote(i))
    i match {
      case LoopIndex(l) =>
        addSoftC(1, l, currentLoop)
      case Def(IntPlus(_,LoopIndex(l))) =>
        addSoftC(1, l, currentLoop)
      case Def(IntPlus(LoopIndex(l),_)) =>
        addSoftC(1, l, currentLoop)
      /*
      case LoopIndex(l) if l.level == currentLevel-1 =>
        val c = dimConstraint(_ == DimX) and blockSizeConstraint(_ % 32 == 0)
        addConstraint(l, c)
			*/
      case _ =>
    }
  }

  private def createLoopInfo(sym: List[Sym[Any]], loop: Any): LoopInfo = {
    val m = LoopInfo(sym, loop, currentLevel, new ArrayBuffer[LoopInfo]())
    if(currentLoop == null)
      loopInfoRoot = m
    else
      currentLoop.nest.append(m)
    m
  }

  // don't need this if a new analysis instance is created each time the analysis is called
  private def resetLoopAnalysis: Unit = {
    softConstraints.clear
    hardConstraints.clear
    currentLevel = 0
    currentLoop = null
    loopInfoRoot = null
    loopAnalysisResult.clear
  }

  /* Entry point for the analysis */
  def start(sym: List[Sym[Any]], rhs: Any, inputs: List[Sym[Any]]): Unit = {
    resetLoopAnalysis
    kernelInputs = inputs

    try {
      rhs match {
        case r: Def[Any] => traverseStm(TP(sym(0),r))
        case r: FatDef => traverseStm(TTP(sym,Nil,r))
      }
      // IR traversal is done. Generate an optimal mapping strategy.
      generateOptMapping
    }
    catch {
      case e: MultiDimMappingFailedException =>
        printInfo("[WARNING] MultiDimMapping Analysis Failed: " + e.msg)
        resetLoopAnalysis
      case e: Exception =>
        throw (e)
    }
    printInfo("Loop Analysis Success")
  }

  def printResult(sym: List[Sym[Any]]): Unit = {
    printInfo("[Mapping result for loop " + sym.mkString("") + "]")
    for ((i,mapping) <- loopAnalysisResult) {
      printInfo("level " + i + " ==> " + mapping)
    }
  }

  override def traverseStm(stm: Stm): Unit =  {
    val rhs = stm.rhs
    val sym = stm.lhs
    rhs match {
      case loop@SimpleFatLoop(sz, v, body) =>
        val m = createLoopInfo(sym,loop)
        analyzeLoopBodies(m)
      case loop@SimpleLoop(sz, v, body) =>
        val m = createLoopInfo(sym,loop)
        analyzeLoopBodies(m)
      case Reflect(loop:AbstractLoop[_],u,es) =>
        val m = createLoopInfo(sym,loop)
        analyzeLoopBodies(m)
      case loop:AbstractLoop[_] =>
        val m = createLoopInfo(sym,loop)
        analyzeLoopBodies(m)
      case DeliteArrayApply(a,i) =>
        processArrayAccess(i)
      case Reflect(DeliteArrayApply(a,i), u, es) =>
        processArrayAccess(i)
      case Reflect(DeliteArrayUpdate(_,i,_), u, es) =>
        processArrayAccess(i)
      case Reflect(StructUpdate(_,_,i,_), u, es) =>
        //TODO: handle nested array update in struct
        processArrayAccess(i.head)
      case _ =>
        traverseFatBlock(blocks(rhs))
    }
  }

  def traverseFatBlock(blocks: List[Block[Any]]): Unit = {
    //TODO: which one?
    traverseBlock(Block(Combine(blocks.map(getBlockResultFull))))
    /*
    focusFatBlock(blocks) {
      focusExactScopeFat(blocks) { levelScope =>
        traverseStmsInBlock(levelScope)
      }
    }
    */
  }

  def collectLoopBody(body: Def[Any]): List[Block[Any]] = {
    body match {
      case elem: DeliteHashCollectElem[_,_,_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      case elem: DeliteHashReduceElem[_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      case elem: DeliteHashIndexElem[_,_] => elem.keyFunc :: elem.cond
      case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func)
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
      case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
    }
  }

  def collectLoopBodyFat(bodies: List[Def[Any]]): List[Block[Any]] = {
    val elemFuncs = bodies flatMap { collectLoopBody }
    elemFuncs.distinct
  }

  def emitValDef(sym: Sym[Any], rhs: String) {}

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
      traverseBlock(body)
      Nil
  }

  def calculateDOP(mapping: HashMap[Int,(Dimension,Int,Span)]): Double = {
    var dop: Double = 1.0
    for((k,v) <- mapping) {
      val s = v._3 match {
        case SpanOne => sizeAtLevel(k)
        case SpanAll => v._2
        case SpanN(size, n) => sizeAtLevel(k) / n
        case Split(size, k) => v._2 * k
        case _ => throw new RuntimeException("Cannot find span type " + v._3)
      }
      dop = dop * s
    }
    dop
  }

  def generateOptMapping: Unit = {

    //TODO: enable passing constraints from user
    //val userConstraints = if (Config.fileGPUMultiDim != "") Source.fromFile(Config.fileGPUMultiDim).getLines().toList else Nil
    val userConstraints: List[String] = Nil

    // permutation set for a loop mapping
    val permSet = HashSet[(Dimension,Int,Span)]()
    for(d <- List(DimX,DimY,DimZ)) {
      for(b <- (4 until 10).map(1 << _)) {
      //for(b <- (0 to 10).map(1 << _)) {
        for(s <- List(SpanOne,SpanAll)) {
          permSet += ((d,b,s))
        }
      }
    }

    // generate all combinations of solution set
    val solutionSet = HashSet[HashMap[Int,(Dimension,Int,Span)]]()
    val maxLevel = allLoops(loopInfoRoot).map(_.level).max + 1
    for(s <- permSet.toList.combinations(maxLevel)) {
      for(p <- s.permutations) {
        val solution = HashMap[Int, (Dimension,Int,Span)]()
        for(l <- 0 until maxLevel) {
          solution += l -> p(l)
        }
        solutionSet += solution
      }
    }

    // filter out by hard constraints
    val s1 = solutionSet.filter { solution =>
      var pass = true

      // thread block size should be min 256 and max 1024
      val size = solution.map(s => s._2._2).reduce(_ * _)
      if(size > 1024 || size < 256) {
        pass = false
      }

      // each level should be mapped to different dimensions
      val dims = solution.map(s => s._2._1).filter(d => d == DimX || d == DimY || d == DimZ)
      if(dims.size != dims.toSet.size) {
        pass = false
      }

      // x precedes y, which precedes z
      if(dims.filter(_ == DimX).size - dims.filter(_ == DimY).size < 0)
        pass = false
      if(dims.filter(_ == DimY).size - dims.filter(_ == DimZ).size < 0)
        pass = false

      // other hard constraints applied at each level
      for(l <- 0 until maxLevel) {
        val (dim,blockSize,span) = solution.get(l).get

        // if size is dynamically calculated or more than 1, then spanone cannot be assigned
        span match {
          //TODO: check all the sizes agree at a certain level
          case SpanOne =>
            val sizes = sizeSymsAtLevel(l)
            assert(sizes.length > 0)
            if(sizes.length > 1) pass = false
            if(!kernelInputs.contains(sizes.head) && !sizes.head.isInstanceOf[Const[Int]]) pass = false
          case _ => //
        }

        // check other dynamically added hard constraints
        hardConstraints.get(l) match {
          case Some(cs) =>
            if (cs exists { c => !c.d(dim) || !c.b(blockSize) || !c.s(span) }) pass = false
          case None => //
        }
      }

      // apply constraints from user (considered as hard constraints)
      userConstraints foreach { line =>
        val tokens: Array[String] = line.split(":")
        val (level,constraint) = (tokens(0).trim.toInt, tokens(1).trim)
        solution.get(level) match {
          case Some(mappingForLevel) =>
            if (pass && constraint.startsWith("dim")) {
              val dim = constraint match {
                case "dimx" => DimX
                case "dimy" => DimY
                case "dimz" => DimZ
                case _ => throw new RuntimeException("Cannot parse user constraint " + constraint)
              }
              if (mappingForLevel._1 != dim) pass = false
            }
            else if (pass && constraint.startsWith("span")) {
              val span = constraint match {
                case "spanall" => SpanAll
                case "spanone" => SpanOne
                case _ => throw new RuntimeException("Cannot parse user constraint " + constraint)
              }
              if (mappingForLevel._3 != span) pass = false
            }
            else if (pass) {
              val size = constraint.toInt
              if (mappingForLevel._2 != size) pass = false
            }
          case None =>
            pass = false
        }
      }

      pass
    }

    // calculate the score of each solution based on the local constraints
    val s2 = s1.map { solution =>
      var score = 0.0
      for(l <- 0 until maxLevel) {
        val (dim,blockSize,span) = solution.get(l).get
        for(c <- softConstraints.filter(_._1.level == l).flatMap(_._2)) {
          if(c.d(dim) && c.b(blockSize) && c.s(span))
            score += c.w
        }
      }
      val dop = calculateDOP(solution)
      (solution, score, dop)
    }

    if(s2.size == 0) {
      throw new MultiDimMappingFailedException("No mapping found to satisfy all constraints")
    }
    else {
      // If there are multiple mappings, pick one with max score and then max DOP
      val maxScore = s2.map(_._2).max
      val resultMapping = s2.filter(_._2 == maxScore).maxBy(_._3)._1
      for(l <- 0 until maxLevel) {
        resultMapping.get(l).get match {
          case (d,b,SpanOne) =>
            val loopSizesAtLevelL = allLoops(loopInfoRoot).filter(_.level == l).map(_.size)
            if(loopSizesAtLevelL.length > 1)
              throw new MultiDimMappingFailedException("SpanOne has multiple sizes at level " + l)
            else
              loopAnalysisResult += l -> (d,b,SpanOne(loopSizesAtLevelL(0)))
          case m@_ => loopAnalysisResult += l -> m
        }
      }
    }

    printInfo("There are multiple possible mappings")
    s2 foreach { s => printInfo(s.toString) }
    printInfo("Printed all possible mappings")

  }

}
