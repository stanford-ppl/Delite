package feattest

import scala.virtualization.lms.internal.Traversal

abstract class Printer extends Traversal {
  import IR._
  override val name = "Printer"
  override def traverseStm(stm: Stm): Unit = {
    super.traverseStm(stm)
    stm match {
      case TP(s,d) => println(strDef(s))
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

abstract class CompileStop extends Printer {
  override val name = "STOP"
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    throw new Exception("STOPPING COMPILIATION")
    b
  }
}
