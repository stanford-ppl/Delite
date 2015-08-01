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


  // Maintain a map of all symbols corresponding to memories
  // which when encountered in DeliteArrayApply, we should emit
  // a read from the value Exp (also a memory location), and use
  // "String" to choose between the two memory locations as the
  // final result.
  // So far has one use case - select between brought in accumulator
  // and existing accumulator for reduction
  val memoryMuxMap = Map[Exp[Any], (Exp[Any], String)]()

  override def quote(x: Exp[Any]) = {
    var found = false
    var i = 0
    var retStr = ""

    while (!found && (i < curSym.length)) {
      if (arbitPrefixMap.contains((curSym(i), x))) {
        retStr = arbitPrefixMap((curSym(i),x)) + "_" + super.quote(x)
        found = true
      }
      i += 1
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
    pw.println(s"amap = $aliasMap")
    pw.println(s"iset = $ignoreSet")

    // MemoryAnalysis
    pw.println(s"[HwGen][Pre-codegen] 3. MemoryAnalysis $b")
    val ma = new MemoryAnalysis {val IR: HwGenMaps.this.IR.type = HwGenMaps.this.IR}
    ma.run(b.asInstanceOf[ma.IR.Block[Any]], aliasMap)
    validMemoryMap = ma.validMemoryMap
    pw.println(s"[HwGen][Pre-codegen] validMemoryMap = $validMemoryMap")
    pw.println(s"[Pre-codegen] Fresh symbol : ${fresh[Int]}")

    // Metapipeline analysis


    // Ready to generate code!

    pw.close()
//    sys.exit(0)
  }

}
