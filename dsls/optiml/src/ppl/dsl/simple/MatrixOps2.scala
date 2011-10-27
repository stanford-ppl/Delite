package ppl.dsl.simple

import ppl.delite.framework.{DeliteApplication, DSLType}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, BaseFatExp, EffectExp}
import scala.virtualization.lms.common.{CGenBase, ScalaGenBase, CGenFat, ScalaGenFat}
import ppl.delite.framework.codegen.delite.DeliteCodegen


trait Matrix[T]

trait MatrixOps2 extends DSLType with Base {

  object Matrix {
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[Double]] = matrix_object_zeros(numRows, numCols)
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

  def matrix_object_zeros[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[A]]
}

trait MatrixOpsExp2 extends MatrixOps2 with BaseFatExp with EffectExp { 
  case class MatrixPlus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends Def[Matrix[A]]

  case class MatrixPPrint[A](x: Exp[Matrix[A]])
    extends Def[Unit]

  case class MatrixObjectZeros[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])
    extends Def[Matrix[A]]

  def matrix_plus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixPlus(x, y)
  def matrix_pprint[A](x: Exp[Matrix[A]]) = reflectEffect(MatrixPPrint(x))
  def matrix_object_zeros[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectZeros[A](numRows,numCols))
}

trait ScalaGenMatrixOps2 extends ScalaGenFat {
  val IR: MatrixOpsExp2
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case MatrixObjectZeros(numRows,numCols) => emitValDef(sym, "Matrix.Zeros(" + quote(numRows) + "," + quote(numCols) + ")")
    case MatrixPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case MatrixPPrint(a) => emitValDef(sym, "" + quote(a) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}

/*
trait CGenMatrixOps2 extends CGenFat {
  val IR: MatrixOpsExp2
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //todo replace the manifest with embedded types
    case MatrixObjectZeros(n1,n2) => emitConstDef("matrix", sym, "Matrix.doubleZeros(" + quote(n1) + "," + quote(n2) +  ")")
    case MatrixPlus(m1,m2) => emitConstDef("matrix", sym, quote(m1) + " + " + quote (m2))
    case MatrixPPrint(m) => emitConstDef("matrix", sym, quote(m) + ".pprint()")
    case _ => super.emitNode(sym, rhs)
  }
}
*/
/*
trait DeliteGenMatrixOps2 extends DeliteCodegen {
  val IR: MatrixOpsExp2
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case MatrixObjectZeros(numRows,numCols) => emitValDef(sym, "Matrix.Zeros(" + quote(numRows) + "," + quote(numCols) + ")")
    case MatrixPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case MatrixPPrint(a) => emitValDef(sym, "" + quote(a) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}
*/