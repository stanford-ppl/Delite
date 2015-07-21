package ppl.delite.framework.codegen.hw

import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

// Analysis passes
import ppl.delite.framework.analysis.PrimitiveReduceAnalysis
import ppl.delite.framework.analysis.MetaPipelineAnalysis
import ppl.delite.framework.analysis.DotPrintAnalysis

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack

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

trait HwCodegen extends GenericCodegen // with ThorIR
{
  /*
   * This HAS to be there with the current codegen design architecture - overloaded emitNode*
   * methods expect their arguments to have types "IR.<blah>".
   */
  // FIXME: Getting a compiler error if extra LMS common stuff isn't mixed in here... Something isn't right.
  // Is GenericCodegen the right thing to be extending here?
//  val IR: LoopsFatExp with ArrayOpsExp with StringOpsExp
  val IR: DeliteOpsExp
  import IR._

  // New stuff from merge with wip-master (some need to be filled in?)
//  def emitHeapMark(): Unit = {}
//  def emitHeapReset(result: List[String]): Unit = {}
//  def emitAbstractFatLoopFooter(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit = {}
//  def emitAbstractFatLoopHeader(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit = {}
//  def syncType(actType: String): String = "??????"
//  def emitWorkLaunch(kernelName: String, rSym: String, allocSym: String, syncSym: String): Unit = {}

  // List of passes to be performed on the graph
//  val passes: ListBuffer[HwPass] = new ListBuffer[HwPass]

  var kernelInputVals: List[Sym[Any]] = Nil
  var kernelInputVars: List[Sym[Any]] = Nil
  def kernelInputs: List[Sym[Any]]= kernelInputVars ++ kernelInputVals
  var kernelOutputs: List[Sym[Any]] = Nil

//  def kernelDeps: List[Module] = hwgraph.getModules(kernelInputs)

  def kernelName = kernelOutputs.map(i => quote(i)).mkString("")

  // --- Methods from GenericCodegen that are overridden/defined ---
  override def deviceTarget: Targets.Value = Targets.Hw
  override def toString = "maxj"

  // TODO: Change to maxj later. Using java for now to get fancy syntax highlighting in vim
  override def fileExtension = "java"

  // Private PrintWriters for BaseKernelLib and TopKernel
  protected var baseKernelLibStream : PrintWriter = null
  protected var topKernelStream : PrintWriter = null
  protected val seenLoops: Set[Sym[Any]] = Set[Sym[Any]]()
  protected var bDir : String = ""

  /*
   * @emitCommonImports: Emit imports that are common across
   * all kernel libraries, topKernel and BaseKernelLib
   */
  private def emitCommonImports(s: PrintWriter) = {
    s.println("package engine;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.stdlib.core.Count.Counter;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.stdlib.core.CounterChain;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.stdlib.core.Count;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.stdlib.core.Count.Params;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.stdlib.memory.Memory;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.Kernel;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.KernelParameters;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.types.base.DFEVar;")
    s.println("import com.maxeler.maxcompiler.v2.utils.MathUtils;")
    s.println("import com.maxeler.maxcompiler.v2.utils.Bits;")
    s.println("import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;")
  }

  private def initBaseKernelLib(s: PrintWriter) = {
    emitCommonImports(s)
    s.println("class BaseKernelLib extends KernelLib {")
    s.println("BaseKernelLib(KernelLib owner) {")
    s.println("  super(owner);")
    s.println("}")

  }

  private def finalizeBaseKernelLib(s: PrintWriter) = {
    s.println("}")
  }

  private def initTopKernel(s: PrintWriter) = {
    emitCommonImports(s)
    s.println("class TopKernel extends Kernel {")
    s.println("TopKernel(KernelParameters parameters) {")
    s.println("  super(parameters);")
  }

  private def finalizeTopKernel(s: PrintWriter) = {
    s.println("}")  // Close constructor brace
    s.println("}")  // Close class brace
  }

  override def initializeGenerator(buildDir:String): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs

    bDir = buildDir
    baseKernelLibStream = new PrintWriter(buildDir + java.io.File.separator + "BaseKernelLib." + fileExtension)
    topKernelStream = new PrintWriter(buildDir + java.io.File.separator + "TopKernel." + fileExtension)

    initBaseKernelLib(baseKernelLibStream)
    initTopKernel(topKernelStream)

    // Initialize all passes here
//    passes.append(new HwPrintPass)

    curSym.push(fresh[Int])
    println(s"Initial curSym: $curSym")

    super.initializeGenerator(buildDir)
  }

  override def finalizeGenerator() = {
    println("[HwCodegen] Inside finalizeGenerator")

//    for (pass <- passes) {
//      pass.doIt(hwgraph.rootNode.asInstanceOf[pass.IR.Module])
//    }

    finalizeBaseKernelLib(baseKernelLibStream)
    finalizeTopKernel(topKernelStream)
    baseKernelLibStream.close()
    topKernelStream.close()
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

    val kernelName = "KLib_" + syms.map(quote).mkString("")
    val kernelSym = syms.map(quote).mkString("")

    emitCommonImports(stream)
    stream.println(s"""class $kernelName  extends BaseKernelLib {""")
    stream.println(s"""$kernelName (KernelLib owner) {""")
    stream.println(s"""  super(owner);""")
    stream.println(s"""}""")

    stream.println(s"void doIt(DFEVar ${kernelSym}_en, DFEVar ${kernelSym}_done) {")
//    if (resourceInfoType != "") {
//      stream.print(resourceInfoSym + ":" + resourceInfoType)
//      if ((vals ++ vars).length > 0) stream.print(",")
//    }
//    stream.print(vals.map(p => quote(p) + ":" + remap(p.tp)).mkString(","))
//    stream.println("")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {
      // Close the doIt function
      stream.println("}")

      // Close the class
      stream.println("}")

  }

  // Every type is remapped to a 'DFEVar' for the hardware backend now
  override def remap[A](m: Manifest[A]) : String = {
    "DFEVar"
  }

  override def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    super.isPrimitiveType(m)
  }

  // TERRIBLE name - returns the String representation of an Exp
  override def quote(x: Exp[Any]) = {
    arbitPrefixMap.getOrElse((curSym.top, x), "") + super.quote(x)
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
  var aliasMap = Map[Exp[Any], Exp[Any]]()
  // When you find 'Exp' while codegenerating for 'Sym', prepend 'String' to 'Exp'
  var arbitPrefixMap = Map[(Sym[Any], Exp[Any]), String]()
  var dblBufMap = Set[Sym[Any]]()
  var curSym = Stack[Sym[Any]]()
  def getBitWidth(sym: Exp[Any]): Int = {
    32
  }

  def getMemorySize(s: Sym[Any]) = {
    Const(32)
  }

  def getStates(N: Int) = {
    val l = 0.until(N).toList
    val lb_total = ListBuffer[List[Int]]()
    for (i <- 0 until l.size) {
         val lb = ListBuffer[List[Int]]()
         lb.append(List(i))
         for (j <- i+1 until l.size) {
           lb.append((lb.last ++ List(j)))
         }
         lb_total.appendAll(lb)
    }
    lb_total.toList
  }

  def stateStr(state:List[Int]) = {
    "S" + state.map( _.toString).reduce(_+_)
  }

  def stateText(state: List[Int], N: Int) = {
    val condStr = state.map("bitVector[" + _ + "]").reduce(_ + " & " + _)
    val max = N-1

    stream.println(s"""IF($condStr) {
      resetBitVector();""")
    if (state.size == 1 && state.max == max) {
      stream.println("stateFF.next = States.DONE;")
    } else {
      if (state.contains(0)) {
        stream.println("counterFF.next <== counterFF + 1;")
        stream.println("IF (counterFF === numIter-1) {")
        stream.print("stateFF.next <== States.")
        if (state.max == max) stream.print(stateStr(state.drop(1))) else stream.print(stateStr(state.drop(1) ++ List(state.max+1)))
          stream.println(";")
          stream.println("} ELSE {")
        stream.print("stateFF.next <== States.")
        if (state.max == max) stream.print(stateStr(state)) else stream.print(stateStr(state ++ List(state.max+1)))
          stream.println(";")
          stream.println("}")
      } else {
        if (state.max == max) stream.print(stateStr(state.drop(1))) else stream.print(stateStr(state.drop(1) ++ List(state.max+1)))
      }
    }
    stream.println("}")
  }


  def emitSM(name: String, st: List[String]) = {
    stream.println("""
package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;
""")

  val smName = name
  val states = getStates(st.size)
  stream.println(s"""class ${smName}_StateMachine extends KernelStateMachine {""")


  // val stateNames = states.map("S" + _.map( _.toString).reduce(_+_))
  val stateNames = states.map(stateStr(_))
  stream.println(s"""
    // States
    enum States {
      ${stateNames.reduce(_ + ",\n" + _) + ",\nDONE"}
    }
  """)

  stream.println("""
    // Constants
    private int numIter;

    // State IO
    private final DFEsmOutput sm_done;
    private final DFEsmOutput sm_count;
    private final DFEsmInput sm_en;
  """)

  for(i <- 0 until st.size) {
    stream.println(s"""
    private final DFEsmInput s${i}_done;
    private final DFEsmOutput s${i}_en;
    """)
  }

  stream.println(s"""
    // State storage
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue counterFF;
    private final DFEsmStateValue[] bitVector;

    private final int numStates = ${st.size};
    // Initialize state machine in constructor
    public ${smName}_StateMachine(KernelLib owner, int numIter) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      sm_done = io.output("sm_done", wireType);
      sm_count = io.output("sm_count", counterType);
      sm_en = io.input("sm_en", wireType);
  """)

  for(i <- 0 until st.size) {
    stream.println(s"""
      s${i}_done = io.input("s${i}_done", wireType);
      s${i}_en = io.output("s${i}_en", wireType);
    """)
  }

  stream.println("""
    // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.S0);
      counterFF = state.value(counterType, 0);

      // Bitvector keeps track of which kernels have finished execution
      // This is a useful hardware synchronization structure to keep
      // track of which kernels have executed/finished execution
      bitVector = new DFEsmStateValue[numStates];
      for (int i=0; i<numStates; i++) {
        bitVector[i] = state.value(wireType, 0);
      }

      // Define constants
      this.numIter = numIter;
    }

    private void resetBitVector() {
      for (int i=0; i<numStates; i++) {
        bitVector[i].next <== 0;
      }
    }
      """)

  stream.println(s"""
    @Override
    protected void nextState() {
      IF(sm_en) {
        // State-agnostic update logic for bitVector
    """)
  for(i <- 0 until st.size) {
    stream.println(s"""
        IF (s${i}_done) {
          bitVector[$i].next <== 1;
        }""")
  }

  stream.println(s"""
        SWITCH(stateFF) {""")

  for(i <- 0 until states.size) {
    val state = states(i)
    val name = stateNames(i)
    stream.println(s"""
          CASE (States.${name}) {""")
      stateText(state, st.size)
    stream.println(s"""
          }""")
  }

  stream.println(s"""
         CASE (States.DONE) {
           resetBitVector();
           stateFF.next <== States.DONE;
         }

         OTHERWISE {
           stateFF.next <== stateFF;
         }
        }
      }
    }""")

  stream.println(s"""
  @Override
    protected void outputFunction() {
      sm_done <== 0;
      sm_count <== counterFF;
      """)

  for (i <- 0 until st.size) {
    stream.println(s"""
      s${i}_en <== 0;""")
  }

  stream.println(s"""
     IF (sm_en) {
       SWITCH(stateFF) {""")
        for(i <- 0 until states.size-1) {
          val state = states(i)
          val name = stateNames(i)
          stream.println(s"""
            CASE (States.$name) {""")
             for (s <- state) {
               stream.println(s"""s${s}_en <== ~(bitVector[$s] | s${s}_done);""")
             }
          stream.println(s"""
                }""")
        }

        stream.println(s"""
          CASE (States.DONE) {
            sm_done <== 1;
          }""")

  stream.println("""
      }
    }
  }
  }
  """)
  }
}

trait HwGenDeliteInternalOps extends HwCodegen {
  val IR: DeliteOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    curSym.push(sym)
    rhs match {
      case DIntPlus(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} + ${quote(rhsAlias)}")

      case DIntMinus(lhs,rhs) =>
  //      hwgraph.add(CSub()(sym, kernelDeps))
  //      stream.println(s"Added CSub()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_sub", List(lhs, rhs)));
      case DIntTimes(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} * ${quote(rhsAlias)}")

  //      hwgraph.add(CMul()(sym, kernelDeps))
  //      stream.println(s"Added CMul()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_mul", List(lhs, rhs)));
      case DIntDivide(lhs,rhs) =>
  //      hwgraph.add(CDiv()(sym, kernelDeps))
  //      stream.println(s"Added CDiv()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_div", List(lhs, rhs)));
      case DLessThan(lhs,rhs) =>
  //      hwgraph.add(Dummy()(sym, kernelDeps))
  //      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_lt", List(lhs, rhs)));
      case DGreaterThan(lhs,rhs) =>
  //      hwgraph.add(Dummy()(sym, kernelDeps))
  //      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_gt", List(lhs, rhs)));
      case _ => super.emitNode(sym,rhs)
  //    case DBooleanNegate(b) => emitValDef(sym, "!" + quote(b))
  //    case DEqual(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
  //    case DNotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
  //    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
  //    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
    }
    curSym.pop
  }
}


/*
 * LMS traits
 * The following traits are usually defined in LMS for the other code
 * generators. I've defined them all in Delite here
 */
//trait HwGenOrderingOps extends HwCodegen
//{
//}
//
//trait HwGenVariables extends HwCodegen
//{
//}
//
//trait HwGenWhile extends HwCodegen
//{
//}
//
//trait HwGenRangeOps extends HwCodegen
//{
//}
//
//
//// FIXME: GenArray and GenString probably should not exist - these are LMS internals unused by Delite (I think)
//trait HwGenArrayOps extends HwCodegen
//{
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case ArrayApply(a, n) =>
////      hwgraph.add(Dummy()(sym, kernelDeps))
//    case ArrayNew(n) =>
//    case ArrayUpdate(a, n, v) =>
//    case ArrayLength(a) =>
//    case _ =>
//      super.emitNode(sym, rhs)
//  }
//}
//
//trait HwGenStringOps extends HwCodegen
//{
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case StringToInt(s) =>
//      stream.println(s"StringToInt($s)")
////      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
////      hwgraph.add(Dummy()(sym, kernelDeps))
//    case _ =>
//      super.emitNode(sym, rhs)
//  }
//}
//
//trait HwGenBooleanOps extends HwCodegen
//{
//}
//
//trait HwGenPrimitiveOps extends HwCodegen
//{
////  val IR: PrimitiveOpsExp
////  import IR._
////
////  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
////    rhs match {
////      case ObjDoublePositiveInfinity() => emitValDef(sym, "INFINITY")
////      case ObjDoubleNegativeInfinity() => emitValDef(sym, "-INFINITY")
////      case _ => super.emitNode(sym, rhs)
////    }
////  }
//}
//
//trait HwGenObjectOps extends HwCodegen
//{
//}
