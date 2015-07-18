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
  def emitHeapMark(): Unit = {}
  def emitHeapReset(result: List[String]): Unit = {}
  def emitAbstractFatLoopFooter(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit = {}
  def emitAbstractFatLoopHeader(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit = {}
  def syncType(actType: String): String = "??????"
  def emitWorkLaunch(kernelName: String, rSym: String, allocSym: String, syncSym: String): Unit = {}

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

    stream.println(s"void doIt(DFEVar en_$kernelSym, DFEVar done_$kernelSym) {")
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

  var aliasMap = Map[Exp[Any], Exp[Any]]()

  def getBitWidth(sym: Exp[Any]): Int = {
    32
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


  def emitSM(name: String, st: List[String], pipeline: Boolean) = {
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

trait HwGenDeliteOps extends HwCodegen with GenericGenDeliteOps
{
  // FIXME: This needs to be changed - temporarily put this here just to make things compile
//  val IR: DeliteOpsExp with LoopsFatExp with ArrayOpsExp with StringOpsExp
  import IR._

  def isPrimitiveReduce(elem: DeliteReduceElem[_]) = {
    val m = elem.mA.toString
    m match {
      case "Int" => true
      case "Float" => true
      case "Double" => true
      case _ => false
    }
  }

  def emitScalarReduceFSM[A](rfunc: Block[A]) = {

    def emitSimpleReduceFn(rfunc: Block[A]) = {
      val analysis = new PrimitiveReduceAnalysis {val IR: HwGenDeliteOps.this.IR.type = HwGenDeliteOps.this.IR}
      val map = analysis.run(rfunc).toList
      if (map.size == 0 || map.size > 1) {
        sys.error(s"Primitive reduce function has more than 1 primitive op! $map")
      }
      map(0)._2 match {
        case DIntPlus(lhs, rhs) =>
          stream.println("counterFF.next <== sm_d + counter;")
        case DIntMinus(lhs, rhs) =>
          stream.println("counterFF.next <== sm_d - counter;")
        case DIntTimes(lhs, rhs) =>
          stream.println("counterFF.next <== sm_d * counter;")
        case DIntDivide(lhs, rhs) =>
          stream.println("counterFF.next <== sm_d / counter;")
        case DLessThan(lhs, rhs) =>
          stream.println("counterFF.next <== sm_d < counter;")
        case DGreaterThan(lhs, rhs) =>
          stream.println("counterFF.next <== sm_d > counter;")
        case _ =>
          sys.error(s"Unknown primitive op ${map(0)._2}")
      }
    }

    stream.println("package engine;")
    stream.println("import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;")
    stream.println("import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;")
    stream.println("import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;")
    stream.println("import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;")
    stream.println("import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;")
    stream.println("import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;")
    stream.println("import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;")

    stream.println(s"class ScalarReduceFSM_${getBlockResult(rfunc)} extends KernelStateMachine {")
    stream.println("// State IO")
    stream.println("  private final DFEsmInput sm_d;")
    stream.println("  private final DFEsmInput en;")
    stream.println("  private final DFEsmOutput sm_q;")
    stream.println("  // Accumulator")
    stream.println("    private final DFEsmStateValue counterFF;")

   stream.println(s"public ScalarReduceFSM_${getBlockResult(rfunc)} (KernelLib owner) {")
   stream.println("   super(owner);")
   stream.println("   DFEsmValueType ffType = dfeUInt(32);")
   stream.println("   DFEsmValueType wireType = dfeBool();")
   stream.println("   sm_d = io.input(\"sm_d\", ffType);")
   stream.println("   en = io.input(\"en\", wireType);")
   stream.println("   sm_q = io.output(\"sm_q\", ffType);")
   stream.println(" counterFF = state.value(ffType, 0);")
   stream.println("}")

   stream.println("@Override")
   stream.println("protected void outputFunction() {")
   stream.println("sm_q <== counterFF;")
   stream.println("}")

   stream.println("@Override")
   stream.println("protected void nextState() {")
   stream.println("IF (en) {")
   emitSimpleReduceFn(rfunc)
   stream.println(" }")
   stream.println("}")
   stream.println("}")
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop =>
      if (Config.debugCodegen) {
        println(s"[codegen] HwGenDeliteOps::emitFatNode::AbstractFatLoop, op = $op, symList = $symList")
      }

      val loopName = symList.map(quote(_)).reduce(_+_)
      val symBodyTuple = symList.zip(op.body)
      println(s"symBodyTuple: $symBodyTuple")
      // Create alias table (boundsSym -> resultSym)
      // Output buffer must have the same name as the loop symbol
      val prevAliasMap = aliasMap
//      aliasMap.clear
      symBodyTuple.foreach {
        case (sym, elem:DeliteCollectElem[_,_,_]) =>
          aliasMap(getBlockResult(elem.buf.alloc)) = aliasMap.getOrElse(sym,sym)
          aliasMap(elem.buf.allocVal) = aliasMap.getOrElse(sym,sym)
          aliasMap(elem.buf.sV) = op.size
          aliasMap(elem.buf.eV) = aliasMap.getOrElse(getBlockResult(elem.func),getBlockResult(elem.func))
        case (sym, elem: DeliteReduceElem[_]) =>
//          aliasMap(elem.rV._1) = getBlockResult(elem.func)
//          aliasMap(elem.rV._2) = sym
//          aliasMap(getBlockResult(elem.rFunc)) = sym
        case _ =>
          throw new Exception("Not handled yet")
      }

      // MetaPipeline analysis - does this loop need a controlling FSM?
      // What should it look like?
      val metapipelineAnalysis = new MetaPipelineAnalysis {val IR: HwGenDeliteOps.this.IR.type = HwGenDeliteOps.this.IR}
      val dotPrintAnalysis = new DotPrintAnalysis {val IR: HwGenDeliteOps.this.IR.type = HwGenDeliteOps.this.IR}


      val bodySMInfo = symBodyTuple.map { t =>
        val bodyMetadata = metapipelineAnalysis.run(t._1, t._2, seenLoops)
        (t._1, bodyMetadata)
      }.groupBy { _._1 }
      .mapValues{ x => List(x(0)._2).flatten.asInstanceOf[List[Sym[Any]]] }
      println(s"bodySMInfo: $bodySMInfo")

      // FSM v/s CounterChain decision here:
      //here If bodySMInfo has atleast one body requiring a SM, emit FSM
      // Else emitCounter
      // Emit counter - this should be agnostic of loop body
      // The counter stride will change based on the parallelism factor
      // Keeping it at 1 for now
      val needsSM = !bodySMInfo.mapValues(_.isEmpty).values.toList.reduce(_&_)

      if (needsSM) {
        if (bodySMInfo.size > 1) {
          sys.error(s"bodySMInfo = $bodySMInfo \nFused loop needs more than one SM, which isn't handled now!")
        }

        val smStages = bodySMInfo.values.toList(0).reverse
        val smIter = quote(op.size)
        val smPipeline = false  // TODO: Set this the output of a Config flag

        val smInputs = smStages.map(x => s"done_${quote(x)}")
        val smOutputs = smStages.map(x => s"en_${quote(x)}")

        stream.println(s"// Loop $loopName needs a SM with the following spec:")
        stream.println(s"""
          // inputs: $smInputs
          // outputs:  $smOutputs
          // count: $smIter
          // pipeline: $smPipeline
          """)

        stream.println(s"""
          SMIO ${loopName}_sm = addStateMachine(\"${loopName}_sm\", new ${loopName}_StateMachine(this, ${smIter}));
          ${loopName}_sm.connectInput(\"sm_en\", en_$loopName); // TODO:Verify if 'en' is right
          DFEVar done_$loopName = ${loopName}_sm.getOutput(\"sm_done\");
          DFEVar ${op.v} = ${loopName}_sm.getOutput(\"sm_count\");""")

          for (idx <- 0 until smInputs.size) {
            val i: String = smInputs(idx)
            val o: String = smOutputs(idx)
            stream.println(s"""
              ${i} = dfeBool().newInstance(this);
              ${loopName}_sm.connectInput(\"s${idx}_done\", ${i});
              DFEVar $o = sm.getOutput(\"s${idx}_en\") & en_$loopName; // TODO: Verify if 'en' is right""")
          }

        val fsmWriter = new PrintWriter(s"${bDir}/${loopName}_StateMachine.${fileExtension}")
        withStream(fsmWriter) {
          emitSM(loopName, smStages.map(x =>quote(x)), smPipeline)
        }
        fsmWriter.close()

      } else {
        stream.println(s"// Loop $loopName does NOT need a SM")
        stream.println(s"CounterChain ${loopName}_chain = control.count.makeCounterChain(en_${loopName});")
        stream.println(s"DFEVar ${quote(op.v)} = ${loopName}_chain.addCounter(${quote(op.size)}, 1);")
        stream.println(s"done_${loopName} <== stream.offset(${loopName}_chain.getCounterWrap(${quote(op.v)}), -1);")

      }

      // In case stuff is fused, emit functions only once
      // Note that counters should be emitted before this
      // as function bodies emitted here will most certainly depend on that
      emitMultiLoopFuncs(op, symList)

      // Generate code for each body
      symBodyTuple.foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          // TODO: Check first if op.size is a const, else assert
//          stream.println(s"Count.Params ${quote(op.v)}_params = control.count.makeParams(addrBits)")
//          stream.println(s"      .withEnable(en)")
//          stream.println(s"      .withMax(${quote(op.size)});")
//
//          stream.println(s"Count.Counter ${quote(op.v)} = control.count.makeCounter(${quote(op.v)}_params);")
//          stream.println(s"done <== stream.offset(${quote(op.v)}.getWrap(), -1);")

          dotPrintAnalysis.run(elem.func, s"collect_$sym.dot")

          emitBlock(elem.buf.alloc)

//          stream.println(s"// The func function - elem.func")
          // emitBlock(elem.func)

          stream.println(s"// The update function - elem.buf.update")
          emitBlock(elem.buf.update)

        case (sym, elem: DeliteReduceElem[_]) =>

          if (isPrimitiveReduce(elem)) {
            // Simple reduces currently use a counter chain
            // This is the way Maxeler's documents describe how to perform
            // the reduction
            stream.println(s"""OffsetExpr ${quote(sym)}_loopLength = stream.makeOffsetAutoLoop(\"${quote(sym)}_loopLength\");""")
            stream.println(s"DFEVar ${quote(sym)}_loopLengthVal = ${quote(sym)}_loopLength.getDFEVar(this, dfeUInt(8));")
            stream.println(s"DFEVar ${quote(sym)}_loopCounter = ${quote(sym)}_chain.addCounter(${quote(sym)}_loopLengthVal, 1);")

            stream.println(s"// The rFunc block")
            stream.println(s"DFEVar ${quote(sym)}_oldAccum = dfeUInt(32).newInstance(this);")
            stream.println(s"DFEVar ${quote(sym)}_zero = constant.var(dfeUInt(32), ${quote(getBlockResult(elem.zero))});")
            emitValDef(elem.rV._2, s"en ? ${quote(getBlockResult(elem.func))} : ${quote(sym)}_zero")
            emitValDef(elem.rV._1, s"${quote(sym)}_oldAccum")
            emitBlock(elem.rFunc)
            stream.println(s"${quote(sym)}_oldAccum <== stream.offset(${quote(getBlockResult(elem.rFunc))}, -${quote(sym)}_loopLength);")

            // Where is the reduced value stored?
            // Current solution: Have DFEVar in BaseKernelLib, assign it to the result from the accumulator
            // This implementation utilizes the FF used to implement the delay (-looplength) to store the
            // accumulation
            baseKernelLibStream.println(s"protected static DFEVar ${quote(sym)};")
            topKernelStream.println(s"""BaseKernelLib.${quote(sym)} = dfeUInt(32).newInstance(this);""")
            stream.println(s"""${quote(sym)} <== ${quote(getBlockResult(elem.rFunc))};""")

//            val tempPw = new PrintWriter("/home/raghu/work/research/mydir2/hyperdsl/delite/framework/delite-test/testFoo.txt")
//            withStream(tempPw) {
//                emitScalarReduceFSM(elem.rFunc)
//            }
//            tempPw.close()
//            val bName = aliasMap(getBlockResult(elem.rFunc))
//            val fsmName = s"rfunc_${bName}"
//            stream.println(s"""SMIO $fsmName = addStateMachine(\"$fsmName\", new ScalarReduceFSM_${bName}(this));""");
//            stream.println(s"""${fsmName}.connectInput(\"sm_d\",  ${quote(aliasMap(elem.rV._1))});""")
//            stream.println(s"""${fsmName}.connectInput(\"en\", en);""")
          } else {
            aliasMap(elem.rV._1) = sym
            aliasMap(elem.rV._2) = getBlockResult(elem.func)
            aliasMap(getBlockResult(elem.rFunc)) = sym
            aliasMap(getBlockResult(elem.zero)) = sym

            stream.println("// Zero block")
            emitBlock(elem.zero)
            stream.println("// End Zero block")
//            emitValDef(elem.rV._2, s"${quote(getBlockResult(elem.func))} : ${quote(getBlockResult(elem.zero))}")
//            emitValDef(elem.rV._1, s"${quote(sym)}_oldAccum")
            stream.println("// rFunc block")
            stream.println(s"// Alias map: $aliasMap")
            emitBlock(elem.rFunc)
            stream.println("// End rFunc block")
//            sys.error(s"Not handling reduces of non-primitive ${elem.mA} types yet!")
          }

        case _ =>
          throw new Exception("Not handled yet")
      }

      symBodyTuple.foreach { t =>
        seenLoops += t._1
      }
      aliasMap = prevAliasMap
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

trait HwGenDeliteInternalOps extends HwCodegen
{
  // FIXME: This needs to be changed
  val IR: DeliteOpsExp with DeliteInternalOpsExp with LoopsFatExp with ArrayOpsExp with StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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
}

trait HwGenDeliteArrayOps extends HwCodegen with BaseGenDeliteArrayOps
{
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val aliasSym = aliasMap.getOrElse(sym, sym).asInstanceOf[Sym[Any]]
    rhs match {
      case DeliteArrayNew(n,m,t) =>
        val aliasN = if (n.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(n.asInstanceOf[Sym[Any]], n) else n
        baseKernelLibStream.println(s"protected static BramLib ${quote(aliasSym)};")
        topKernelStream.println(s"""BaseKernelLib.${quote(aliasSym)} = new BramLib(this, \"${quote(aliasSym)}\", ${quote(aliasN)});""")

      case DeliteArrayApply(arr, idx) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx, idx)
        stream.println(s"${quote(aliasArr)}.raddr <== ${quote(aliasIdx)};")
        emitValDef(aliasSym, s"${quote(aliasArr)}.rdata")

      case DeliteArrayUpdate(arr,idx,v) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx.asInstanceOf[Sym[Any]], idx)
        val aliasV = aliasMap.getOrElse(v.asInstanceOf[Sym[Any]], v)
        stream.println(s"${quote(aliasArr)}.waddr <== ${quote(aliasIdx)};")
        stream.println(s"${quote(aliasArr)}.wdata <== ${quote(aliasV)};")

      case DeliteArrayLength(arr) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        stream.println("// code for DeliteArrayLength goes here")
        emitValDef(aliasSym, "DeliteArrayLength")
      case _=>
        super.emitNode(sym, rhs)
    }
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
