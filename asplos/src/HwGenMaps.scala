package asplos
import asplos._

import ppl.delite.framework.codegen.hw.HwCodegen
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import java.io.PrintWriter
import ppl.delite.framework.analysis.PrintAnalysis

trait HwGenMaps extends HwCodegen with GenericHelper {
  val IR: PPLOpsExp
  import IR._


  // If you see _1, emit _2
  var aliasMap = Map[Exp[Any], Exp[Any]]()

  def aslt(e: Exp[Any]): Exp[Any] = {
    if (aliasMap.contains(e)) {
      aslt(aliasMap(e))
    } else {
      e
    }
  }

  // Don't do anything when you see a sym in this set
  // Kind-of redundant, but is nice to be explicit
  val ignoreSet = Set[Exp[Any]]()

  // If you see 'Exp' while codegenerating for Loop 'Sym', prepend 'String' to 'Exp'
  var arbitPrefixMap = Map[(Sym[Any], Exp[Any]), String]()
  // dbl buf writer -> numrports map used during DeliteArray*
  var dblBufMap = Map[Sym[Any], Int]()

  // Maps to handle array2D structs
  var reverseStructMap = Map[Exp[Any],Exp[Any]]()
  var structInfoMap = Map[(Exp[Any],String), Exp[Any]]()
  val validMemorySet = Set[Exp[Any]]()
  val validMemoryMap = Map[Exp[Any], MemInfo]()

  val outerLoopMap = Map[Exp[Any], List[Exp[Any]]]()

  // Maintain a map of all symbols corresponding to memories
  // which when encountered in DeliteArrayApply, we should emit
  // a read from the value Exp (also a memory location), and use
  // "String" to choose between the two memory locations as the
  // final result.
  // So far has one use case - select between brought in accumulator
  // and existing accumulator for reduction
  val memoryMuxMap = Map[Exp[Any], (Exp[Any], String)]()
  val phantomMemReadMap = Map[Exp[Any], (Exp[Any], String)]()

  // Metapipeline idx -> metapipeline Sym map
  val mpIdxMap = Map[Exp[Any], Sym[Any]]()

  // Metapipeline sym -> stages in the metapipeline
  // The use case is as follows:
  // 1. Look up sym in mpIdxMap (above) to find which metapipeline it is coming from
  // 2. In the curSym stack (top to bottom), find the first stage that exists
  //    for that metapipeline in mpStageInfo. That's the prefix that needs to be added
  val mpStageMap = Map[Sym[Any], List[List[Sym[Any]]]]()

  // Map of syms which need a delay (in stream.offset) before
  // updating. Required to handle reduces of collections
  val delayWriteMap = Map[Exp[Any], String]()

  override def quote(x: Exp[Any]) = {
    var found = false
    var i = 0
    var retStr = ""

    if (mpIdxMap.contains(x)) {
      val mp = mpIdxMap(x)
      val mpStages = mpStageMap(mp)
      while (!found && i < curSym.length) {
        if (mpStages.flatten.contains(curSym(i))) {
          found = true
          val stage = mpStages.filter ( s => s.contains(curSym(i)))  // There's going to be just one
          val stageName = stage(0).map(quote(_)).reduce(_+_)
          retStr = stageName + "_" + super.quote(x)
        }
        i += 1
      }
    } else {
      while (!found && (i < curSym.length)) {
        if (arbitPrefixMap.contains((curSym(i), x))) {
          retStr = arbitPrefixMap((curSym(i),x)) + "_" + super.quote(x)
          found = true
        }
        i += 1
      }
    }
    if (!found) {
      retStr = super.quote(x)
    }

    retStr

//    arbitPrefixMap.getOrElse((curSym.top, x), "") + super.quote(x)
  }

  override def preCodegenAnalysis(b: Block[Any]) = {
    val pw = new PrintWriter("/home/raghu/work/research/mydir2/hyperdsl/delite/asplos/scratch.txt")
    pw.println(s"[Pre-codegen] Fresh symbol : ${fresh[Int]}")

    // Append analysis passes here
    pw.println(s"[HwGen][Pre-codegen] 1. Printing $b")
    val pa = new PrintAnalysis {val IR: HwGenMaps.this.IR.type = HwGenMaps.this.IR}
    pa.run(b.asInstanceOf[pa.IR.Block[Any]], "Entire program", pw)

    // SymAliasAnalysis
    pw.println(s"[HwGen][Pre-codegen] 2. SymAliasAnalysis $b")
    val saa = new SymAliasAnalysis {val IR: HwGenMaps.this.IR.type = HwGenMaps.this.IR}
    saa.run(b.asInstanceOf[saa.IR.Block[Any]])
    aliasMap = saa.aliasMap
    ignoreSet = saa.ignoreSet
    reverseStructMap = saa.reverseStructMap
    structInfoMap = saa.structInfoMap
    pw.println(s"amap = $aliasMap")
    pw.println(s"iset = $ignoreSet")

    // MemoryAnalysis
    pw.println(s"[HwGen][Pre-codegen] 3. MemoryAnalysis $b")
    val ma = new MemoryAnalysis {val IR: HwGenMaps.this.IR.type = HwGenMaps.this.IR}
    ma.run(b.asInstanceOf[ma.IR.Block[Any]], aliasMap)
    validMemoryMap = ma.validMemoryMap
    outerLoopMap = ma.outerLoopMap
    pw.println(s"[HwGen][Pre-codegen] validMemoryMap = $validMemoryMap")
    pw.println(s"[Pre-codegen] Fresh symbol : ${fresh[Int]}")

    // Metapipeline analysis


    // Ready to generate code!

    pw.close()
//    sys.exit(0)
  }

  protected def emitBlockLoader(
  sym: Sym[Any], srcDimBurst: String,
  i: String, jburst: String,
  typ: String, srcBurstOffset: String,
  dim1: String, dim2: String,
  forceLdSt: String = "constant.var(true)"
  ) = {

  val name = quote(sym)
baseKernelLibStream.println(s"protected static DFEVar ${name}_isLoading;")
stream.println(s"""
      // Block loader
      DFEVar ${name}_forceLdSt = $forceLdSt;
      ${name}_isLoading = dfeBool().newInstance(this);
      DFEVar ${name}_waddr = dfeUInt(MathUtils.bitsToAddress(${dim1}*${dim2})).newInstance(this);
      DFEVar ${name}_wdata = ${typ}.newInstance(this);
      DFEVar ${name}_wen = dfeBool().newInstance(this);
      new BlockLoaderLib (
        owner,
        ${name}_en, ${name}_done,
        ${name}_isLoading, ${name}_forceLdSt,
        ${srcDimBurst}, ${typ},
        ${i}, ${jburst},
        ${srcBurstOffset}, \"${name}\",
        ${dim1}, ${dim2},
        ${name}_waddr, ${name}_wdata, ${name}_wen
      );
      ${name}.connectWport(${name}_waddr, ${name}_wdata, ${name}_wen);""")
  }

  protected def emitBlockStorer(
  sym: Sym[Any], srcSym: Sym[Any], srcDimBurst: String,
  i: String, jburst: String,
  typ: String, srcBurstOffset: String,
  dim1: String, dim2: String,
  forceLdSt: String = "constant.var(true)"
  ) = {

  val name = quote(sym)
baseKernelLibStream.println(s"protected static DFEVar ${name}_isLoading;")
stream.println(s"""
      // Block storer
      optimization.pushPipeliningFactor(0);
      DFEVar ${name}_forceLdSt = $forceLdSt;
      optimization.popPipeliningFactor();
      ${name}_isLoading = dfeBool().newInstance(this);
      DFEVar ${name}_raddr = dfeUInt(MathUtils.bitsToAddress(${dim1}*${dim2})).newInstance(this);
      DFEVar ${name}_rdata = ${quote(srcSym)}.connectRport(${name}_raddr, ${name}_done);
      new BlockStorerLib (
        owner,
        ${name}_en, ${name}_done,
        ${name}_isLoading, ${name}_forceLdSt,
        ${srcDimBurst}, ${typ},
        ${i}, ${jburst},
        ${srcBurstOffset}, \"${name}\",
        ${dim1}, ${dim2},
        ${name}_raddr, ${name}_rdata
      );""")
  }
}


