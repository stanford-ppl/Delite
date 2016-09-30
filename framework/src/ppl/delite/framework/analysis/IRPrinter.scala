package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.{Effects,Traversal}
import scala.virtualization.lms.common.BaseFatExp

trait IRPrinter extends Traversal {
  import IR._
  override val name = "Printer"

  var level = 0

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    msg(".."*level + s"$lhs = $rhs")
    level += 1
    super.traverse(lhs, rhs)
    level -= 1
  }

  override def traverse(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef) = {
    msg(lhs.mkString("(", ",", ")") + " = " + rhs.toString)
    msg("   " + mhs.mkString("\n   "))
  }

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    msg(name + "\n------------------------------------")
    super.preprocess(b)
  }
}

// Print IR + metadata for each encountered symbol
trait IRPrinterPlus extends Traversal {
  val IR: BaseFatExp with Effects
  import IR._
  override val name = "PrinterPlus"
  var level = 0
  debugMode = true

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    msg(".."*level + s"$lhs = $rhs")
    level += 1
    getProps(lhs).foreach{props => props.data.foreach{(k,m) => debug(".."*level + readable(k) + makeString(m)) }}
    level -= 1

    level += 1
    super.traverse(lhs, rhs)
    level -= 1
  }

  override def traverse(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef) = {
    msg(".."*level + lhs.mkString("(", ",", ")") + " = " + rhs.toString)
    msg(".."*level + " - " + mhs.mkString("\n   "))
    lhs.foreach{ sym =>
      getProps(sym).foreach{props => props.data.foreach{(k,m) => debug(".."*level + readable(k) + makeString(m)) }}
    }
  }

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    msg(name + "\n------------------------------------")
    super.preprocess(b)
  }
}

trait HardStop extends Traversal {
  import IR._

  override def run[A:Manifest](b: Block[A]) = {
    msg("Hard stop reached during IR traversal")
    sys.exit(-1)
  }
}
