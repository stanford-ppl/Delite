package ppl.dsl.simple

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
import java.io.PrintWriter
import ppl.delite.framework.embedded.scala.CodeGeneratorCMisc
import ppl.delite.framework.codegen.c.CodeGeneratorCBase
import scala.virtualization.lms.common.EffectExp


trait Matrix[T]

trait MatrixOps2 extends DSLType { this: DeliteApplication =>

  object Matrix {
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[Double]] = matrix_new(numRows, numCols)
  }

  implicit def matRepArith[A](x: Rep[Matrix[A]]) = new matRepCls(x)
  //implicit def varToRepMatOps[A](x: Var[Matrix[A]]) : matRepCls[A]

  class matRepCls[A](x: Rep[Matrix[A]]) {
    def +(y: Rep[Matrix[A]])(implicit mA: Manifest[A], n: Numeric[A]) = matrix_plus(x,y)
    def pprint = matrix_pprint(x)    
  }

  // class defs
  def matrix_plus[A:Manifest:Numeric](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_pprint[A](x: Rep[Matrix[A]]): Rep[Unit]

  def matrix_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[A]]
}

trait MatrixOpsExp2 extends MatrixOps2 { this: DeliteApplication =>
  case class MatrixPlus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends Def[Matrix[A]]

  case class MatrixPPrint[A](x: Exp[Matrix[A]])
    extends Def[Unit]

  case class MatrixNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])
    extends Def[Matrix[A]]

  def matrix_plus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixPlus(x, y)
  def matrix_pprint[A](x: Exp[Matrix[A]]) = reflectEffect(MatrixPPrint(x))
  def matrix_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixNew[A](numRows,numCols))

  targets.get("Scala").getOrElse(
    throw new RuntimeException("Couldn't find Scala code generator")
  ) .generators += new CodeGeneratorScalaMatrix {
    val intermediate: MatrixOpsExp2.this.type = MatrixOpsExp2.this
  }

  targets.get("C").getOrElse(
    throw new RuntimeException("Couldn't find C code generator")
  ) .generators += new CodeGeneratorCMatrix {
    val intermediate: MatrixOpsExp2.this.type = MatrixOpsExp2.this
  }

}

trait CodeGeneratorScalaMatrix extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with MatrixOpsExp2 with EffectExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case MatrixNew(numRows,numCols) => emitValDef(sym, "new matrix")
    case MatrixPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case MatrixPPrint(a) => emitValDef(sym, "exit(" + quote(a) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CodeGeneratorCMatrix extends CodeGeneratorCBase {

  val intermediate: DeliteApplication with MatrixOpsExp2 with EffectExp
  import intermediate._

  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    //todo replace the manifest with embedded types
    case MatrixNew(n1,n2) => emitConstDef("matrix", sym, "Matrix.zeros(" + quote(n1) + ")")
    case MatrixPlus(m1,m2) => emitConstDef("matrix", sym, quote(m1) + " + " + quote (m2))
    case MatrixPPrint(m) => emitConstDef("matrix", sym, quote(m) + ".pprint()")
    case _ => super.emitNode(sym, rhs)
  }
}
