package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.{Effects,Traversal}
import scala.virtualization.lms.common.BaseFatExp

trait IRPrinter extends Traversal {
  import IR._
  override val name = "Printer"

  var level = 0

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(s,d) =>
        println(".."*level + s"$s = $d")
        level += 1
        super.traverseStm(stm)
        level -= 1

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
  var level = 0

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(s,d) =>
        println(".."*level + s"$s = $d")
        getProps(s).foreach{m => println(".."*level+s"$s" + makeString(m)) }

        level += 1
        super.traverseStm(stm)
        level -= 1

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
