package ppl.delite.framework.codegen.hw

import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

/*
 * Delite Hardware code generator - This file contains all the traits that implement
 * the backend for hardware generation. All the traits that implement code generators
 * for specific IR nodes (with overridden emitNode methods) along with the base
 * code generator are defined here. Changes to the hardware codegen must be done here
 *
 *               +--------------------+
 *               |   GenericCodegen   |
 *               +---------+----------+
 *                         |
 *                         |
 *                   +-----v------+        +------------------------+
 *                   | HwCodegen  |        |  GenericGenDeliteOps   |
 *                   +-+---------++        +-+----------------------+
 *                     |         |           |
 *                     |         |           |
 * +-------------------v--+     +v-----------v----+
 * |HwGenDeliteInternalOps|     | HwGenDeliteOps  |
 * +----------------------+     +-----------------+
 *
 */

trait HwCodegen extends GenericCodegen with ThorIR {
  /*
   * This HAS to be there with the current codegen design architecture - overloaded emitNode*
   * methods expect their arguments to have types "IR.<blah>".
   */
  val IR: Expressions
  import IR._

  // Hardware intermediate representation graph
  val hwgraph: HwGraph = new HwGraph

  // List of passes to be performed on the graph
  val passes: ListBuffer[HwPass] = new ListBuffer[HwPass]

  var kernelInputVals: List[Sym[Any]] = Nil
  var kernelInputVars: List[Sym[Any]] = Nil
  def kernelInputs: List[Sym[Any]]= kernelInputVars ++ kernelInputVals
  var kernelOutputs: List[Sym[Any]] = Nil

  def kernelDeps: List[Module] = hwgraph.getModules(kernelInputs)

  def kernelName = kernelOutputs.map(i => quote(i)).mkString("")

  // --- Methods from GenericCodegen that are overridden/defined ---
  override def deviceTarget: Targets.Value = Targets.Hw
  override def toString = "hw"

  override def fileExtension = "thor"

  override def initializeGenerator(buildDir:String): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs

    // Initialize all passes here
    passes.append(new HwPrintPass)

    super.initializeGenerator(buildDir)
  }

  override def finalizeGenerator() = {
    println("[HwCodegen] Inside finalizeGenerator")

    for (pass <- passes) {
      pass.doIt(hwgraph.rootNode.asInstanceOf[pass.IR.Module])
    }

  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    kernelInputVals = vals
    kernelInputVars = vars
    kernelOutputs = syms
  }

  def emitSource[A:Manifest](args: List[Sym[_]], body: Block[A], functionName: String, out: PrintWriter) = {
    Nil
  }

  // emitKernelHeader: This function is called for every kernel, and contains code that should
  // be generated in all the kernel files. Basing this off of ScalaCodegen's implementation
  // for now
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {

//    def kernelSignature: String = {
//      val out = new StringBuilder
//      out.append(kernelName + "(")
//      out.append(")")
//      out.toString
//    }
//    stream.println("object " + kernelSignature + " {")

    val kernelName = syms.map(quote).mkString("")
    stream.println("object kernel_" + kernelName + " {")
    stream.print("def apply(")
    if (resourceInfoType != "") {
      stream.print(resourceInfoSym + ":" + resourceInfoType)
      if ((vals ++ vars).length > 0) stream.print(",")
    }
    stream.print(vals.map(p => quote(p) + ":" + remap(p.tp)).mkString(","))

    // variable name mangling
    if (vals.length > 0 && vars.length > 0){
      stream.print(",")
    }
    // TODO: remap Ref instead of explicitly adding generated.scala
    if (vars.length > 0) {
      stream.print(vars.map(v => quote(v) + ":" + "generated.scala.Ref[" + remap(v.tp) +"]").mkString(","))
    }
    if (resultIsVar){
      stream.print("): " + "generated.scala.Ref[" + resultType + "] = {")
    }
    else {
      stream.print("): " + resultType + " = {")
    }

    stream.println("")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {
      stream.println("}")
  }

  // Every type is remapped to a 'Module' for the hardware backend now
  override def remap[A](m: Manifest[A]) : String = {
    "Module"
  }

  override def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    super.isPrimitiveType(m)
  }

  // TERRIBLE name - returns the String representation of an Exp
  override def quote(x: Exp[Any]) = {
    super.quote(x)
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  override def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + ";")
  }

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(quote(sym) + " = " + rhs + ";")
  }
  // --- End Methods from GenericCodegen that are overridden/defined ---

  // --- Begin methods specific to HwCodegen ---
  // getBitWidth: Returns an integer value representing
  // the maximum number of bits required to store this symbol
  // Currently just returning '32', but should be hooked up
  // to the bitwidth analysis results
  def getBitWidth(sym: Exp[Any]): Int = {
    32
  }

  // buildClbCall: Construct a String that represents a combinational
  // logic block instantiation. This is a helper function that makes
  // other sub-traits of HwCodegen easier to read
  def buildClbCall(clbop: String, inputs: List[Exp[Any]]): String = {
    val bitWidths = inputs.map (i => getBitWidth(i))
    val maxBitWidth = bitWidths.max
    val inputNames = inputs.map (i => quote(i)).mkString(",")
    val params : String = List(maxBitWidth, "List(" + inputNames + ")").mkString(",")
    clbop + "(" + params + ")"
  }
}

trait HwGenDeliteOps extends HwCodegen with GenericGenDeliteOps {
  val IR: DeliteOpsExp
  import IR._

  // New stuff from merge with wip-master (some need to be filled in?)
  def emitHeapMark(): Unit = {}
  def emitHeapReset(result: List[String]): Unit = {}
  def emitAbstractFatLoopFooter(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit = {}
  def emitAbstractFatLoopHeader(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit = {}
  def syncType(actType: String): String = "??????"
  def emitWorkLaunch(kernelName: String, rSym: String, allocSym: String, syncSym: String): Unit = {}

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop =>
      if (Config.debugCodegen) {
        println(s"[codegen] HwGenDeliteOps::emitFatNode::AbstractFatLoop, op = $op, symList = $symList")
      }

      val symBodyTuple = symList.zip(op.body)

      symBodyTuple.foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          stream.println(s"// Pseudocode for $elem")
          emitBlock(elem.func)
          emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
          emitValDef(elem.buf.allocVal, quote(sym)+"_data")
          stream.println(quote(elem.buf.allocVal) + "(" + quote(op.v) + ") = " + quote(elem.buf.eV))

        case (sym, elem: DeliteReduceElem[_]) =>
          stream.println(s"// Pseudocode for $elem")
        case _ =>
          throw new Exception("Not handled yet")
      }
    case _ => super.emitFatNode(symList, rhs)
  }

  // Abstract methods in GenericGenDeliteOps defined here
  def quotearg(x: Sym[Any]) = {
  }

  def quotetp(x: Sym[Any]) = {
  }

  def methodCall(name:String, inputs: List[String] = Nil): String = {
    "methodCall"
  }

  def emitMethodCall(name:String, inputs: List[String]): Unit = {
  }

  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit): Unit = {
    if (Config.debugCodegen) {
      println(s"[codegen] [HwGenDeliteOps] emitMethod ($name, $outputType, $inputs)")
    }
  }

  def createInstance(typeName:String, args: List[String] = Nil): String = {
    "createInstance"
  }

  def fieldAccess(className: String, varName: String): String = {
    "fieldAccess"
  }

  def releaseRef(varName: String): Unit = {
  }

  def emitReturn(rhs: String) = {
  }

  def emitFieldDecl(name: String, tpe: String) = {
  }

  def emitClass(name: String)(body: => Unit) = {
  }

  def emitObject(name: String)(body: => Unit) = {
  }

  def emitValDef(name: String, tpe: String, init: String): Unit = {
  }

  def emitVarDef(name: String, tpe: String, init: String): Unit = {
  }

  def emitAssignment(name: String, tpe: String, rhs: String): Unit = {
  }

  def emitAssignment(lhs: String, rhs: String): Unit = {
  }

  def emitAbstractFatLoopHeader(className: String, actType: String): Unit = {
    if (Config.debugCodegen) {
      println(s"[codegen] Calling emitAbstractFatLoopHeader on classname: $className, actType: $actType")
    }
  }

  def emitAbstractFatLoopFooter(): Unit = {
    if (Config.debugCodegen) {
      println(s"[codegen] Calling emitAbstractFatLoopFooter")
    }
  }

  def castInt32(name: String): String = {
    "castInt32"
  }

  def refNotEq: String = {
    "refNotEq"
  }

  def nullRef: String = {
    "nullRef"
  }

  def arrayType(argType: String): String = {
    "arrayType"
  }

  def arrayApply(arr: String, idx: String): String = {
    "arrayApply"
  }

  def newArray(argType: String, size: String): String = {
    "newArray"
  }

  def hashmapType(argType: String): String = {
    "hashmapType"
  }

  def typeCast(sym: String, to: String): String = {
    "typeCast"
  }

  def withBlock(name: String)(block: => Unit): Unit = {
  }
}

trait HwGenDeliteInternalOps extends HwCodegen {
  val IR: DeliteOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DIntPlus(lhs,rhs) =>
      stream.println(s"DIntPlus($lhs, $rhs)")
      val depSyms = kernelInputs ++ List(lhs, rhs)
      println(s"depSyms = $depSyms")
      val allDeps = hwgraph.getModules(depSyms)
      println(s"allDeps = $allDeps")
      hwgraph.add(CAdd()(sym, allDeps))
      stream.println(s"Added Cadd()($sym, $allDeps) to hwgraph")
      println(s"Added Cadd()($sym, $allDeps) to hwgraph")
    case DIntMinus(lhs,rhs) =>
      hwgraph.add(CSub()(sym, kernelDeps))
      stream.println(s"Added CSub()($sym, $kernelDeps) to hwgraph")
//      emitValDef(sym, buildClbCall("clb_sub", List(lhs, rhs)));
    case DIntTimes(lhs,rhs) =>
      hwgraph.add(CMul()(sym, kernelDeps))
      stream.println(s"Added CMul()($sym, $kernelDeps) to hwgraph")
//      emitValDef(sym, buildClbCall("clb_mul", List(lhs, rhs)));
    case DIntDivide(lhs,rhs) =>
      hwgraph.add(CDiv()(sym, kernelDeps))
      stream.println(s"Added CDiv()($sym, $kernelDeps) to hwgraph")
//      emitValDef(sym, buildClbCall("clb_div", List(lhs, rhs)));
    case DLessThan(lhs,rhs) =>
      hwgraph.add(Dummy()(sym, kernelDeps))
      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
//      emitValDef(sym, buildClbCall("clb_lt", List(lhs, rhs)));
    case DGreaterThan(lhs,rhs) =>
      hwgraph.add(Dummy()(sym, kernelDeps))
      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
//      emitValDef(sym, buildClbCall("clb_gt", List(lhs, rhs)));
    case _ => super.emitNode(sym,rhs)
//    case DBooleanNegate(b) => emitValDef(sym, "!" + quote(b))
//    case DEqual(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
//    case DNotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
//    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
//    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
  }
}

trait HwGenDeliteArrayOps extends HwCodegen {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteArrayNew(n,m,t) =>
      emitValDef(sym, s"bram($n, $m, $t)")
    case _=>
      super.emitNode(sym, rhs)
  }
}


/*
 * LMS traits
 * The following traits are usually defined in LMS for the other code
 * generators. I've defined them all in Delite here
 */
trait HwGenOrderingOps extends HwCodegen
trait HwGenVariables extends HwCodegen
trait HwGenWhile extends HwCodegen
trait HwGenRangeOps extends HwCodegen

trait HwGenArrayOps extends HwCodegen {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(a, n) =>
      hwgraph.add(Dummy()(sym, kernelDeps))
    case ArrayNew(n) =>
    case ArrayUpdate(a, n, v) =>
    case ArrayLength(a) =>
    case _ =>
      super.emitNode(sym, rhs)
  }
}

trait HwGenStringOps extends HwCodegen {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringToInt(s) =>
      stream.println(s"StringToInt($s)")
      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
      hwgraph.add(Dummy()(sym, kernelDeps))
    case _ =>
      super.emitNode(sym, rhs)
  }
}

trait HwGenBooleanOps extends HwCodegen

trait HwGenPrimitiveOps extends HwCodegen
{
//  val IR: PrimitiveOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
//    rhs match {
//      case ObjDoublePositiveInfinity() => emitValDef(sym, "INFINITY")
//      case ObjDoubleNegativeInfinity() => emitValDef(sym, "-INFINITY")
//      case _ => super.emitNode(sym, rhs)
//    }
//  }
}

trait HwGenObjectOps extends HwCodegen

trait HwGenDSLOps extends HwCodegen
