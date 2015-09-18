package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.Traversal

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
      case _ => //
    }
  }

  override def run[A:Manifest](b: Block[A]) = {
    println(name + "\n------------------------------------")
    super.run(b)
  }

}