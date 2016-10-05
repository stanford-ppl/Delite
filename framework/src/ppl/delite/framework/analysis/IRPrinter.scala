package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.{Effects,Traversal}
import scala.virtualization.lms.common.BaseFatExp

// Print IR + metadata for each encountered symbol
trait IRPrinter extends Traversal {
  val IR: BaseFatExp with Effects
  import IR._
  override val name = "PrinterPlus"
  debugMode = true

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    if (blocks(rhs).nonEmpty)
      msgs(s"$lhs = $rhs {")
    else
      msgs(s"$lhs = $rhs")
    debugs(s" -Type = ${readable(lhs.tp.runtimeClass)}")
    getProps(lhs).foreach{props => props.data.foreach{(k,m) => debugs(" -" + readable(k) + makeString(m)) }}

    blocks(rhs).zipWithIndex.foreach{case (blk,i) =>
      tab += 1
      msgs(s"block $i: $blk {")
      traverseBlock(blk)
      msgs(s"} // End of $lhs block #$i")
      tab -= 1
    }
    if (blocks(rhs).nonEmpty) msgs(s"} // End of $lhs")
  }

  override def traverse(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef) = {
    msgs(lhs.mkString("(", ",", ")") + " = " + rhs.toString)
    msgs(" - " + mhs.mkString("\n   "))
    lhs.foreach{ sym =>
      getProps(sym).foreach{props => props.data.foreach{(k,m) => debugs(" -" + readable(k) + makeString(m)) }}
    }
  }

  // Only run traversal if debugging/verbose mode is enabled
  override def run[A:Manifest](b: Block[A]) = if (debugMode || verboseMode) super.run(b) else b
}



trait HardStop extends Traversal {
  import IR._

  override def run[A:Manifest](b: Block[A]) = {
    msg("Hard stop reached during IR traversal")
    sys.exit(-1)
  }
}
