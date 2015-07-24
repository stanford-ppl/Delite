package asplos
import asplos._

import ppl.delite.framework.codegen.hw.HwCodegen
import scala.collection.mutable.Map
import scala.collection.mutable.Set

trait HwGenMaps extends HwCodegen {
  val IR: PPLOpsExp
  import IR._

  // If you see _1, emit _2
  var aliasMap = Map[Exp[Any], Exp[Any]]()
  // If you see 'Exp' while codegenerating for Loop 'Sym', prepend 'String' to 'Exp'
  var arbitPrefixMap = Map[(Sym[Any], Exp[Any]), String]()
  var dblBufMap = Set[Sym[Any]]()

  // Maps to handle array2D structs
  var reverseStructMap = Map[Exp[Any],Exp[Any]]()
  var structInfoMap = Map[(Exp[Any],String), Exp[Any]]()
  val validMemorySet = Set[Exp[Any]]()
  override def quote(x: Exp[Any]) = {
    if (arbitPrefixMap.contains((curSym.top, x))) {
      arbitPrefixMap((curSym.top,x)) + "_" + super.quote(x)
    } else {
      super.quote(x)
    }
//    arbitPrefixMap.getOrElse((curSym.top, x), "") + super.quote(x)
  }
}
