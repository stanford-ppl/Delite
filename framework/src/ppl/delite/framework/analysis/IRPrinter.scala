package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.{Effects,Traversal}
import scala.virtualization.lms.common.BaseFatExp

trait IRPrinter extends Traversal {
  import IR._
  override val name = "Printer"

  override def traverseStm(stm: Stm): Unit = {
    super.traverseStm(stm)
    stm match {
      case TP(s,d) => println(s"$s = $d")
      case TTP(syms, mhs, d) =>
        println(syms.mkString("(", ",", ")") + " = " + d.toString)
        println("   " + mhs.mkString("\n   "))
    }
  }

  override def run[A:Manifest](b: Block[A]) = {
    println(name + "\n------------------------------------")
    super.run(b)
  }
}

// Print IR + metadata for each encountered symbol
trait IRPrinterPlus extends Traversal {
  val IR: BaseFatExp with Effects
  import IR._
  override val name = "PrinterPlus"

  override def traverseStm(stm: Stm): Unit = {
    super.traverseStm(stm) // Traverses blocks
    stm match {
      case TP(s,d) =>
        println(s"$s = $d")
        getProps(s).foreach{m => println(s"$s" + makeString(m)) }

      case TTP(syms, mhs, d) =>
        println(syms.mkString("(", ",", ")") + " = " + d.toString)
        println("   " + mhs.mkString("\n   "))
        syms.foreach{ sym =>
          getProps(sym).foreach{m => println(s"$sym" + makeString(m)) }
        }
    }
  }

  override def run[A:Manifest](b: Block[A]) = {
    println(name + "\n------------------------------------")
    super.run(b)
  }
}