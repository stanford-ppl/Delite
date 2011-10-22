package ppl.dsl.optila.vector

import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optila.{MatrixCol}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

trait MatrixColOps extends Base with OverloadHack { this: OptiLA =>

  //def infix_index[A](x: Rep[MatrixCol[A]])(implicit mA: Manifest[A], o: Overloaded1) = matrixcol_index(x)

  // class defs
  //def matrixcol_index[A:Manifest](x: Rep[MatrixCol[A]]): Rep[Int]
}

trait MatrixColOpsExp extends MatrixColOps with BaseExp { this: OptiLAExp =>

  // implemented via method on real data structure
  //case class MatrixColIndex[A:Manifest](x: Exp[MatrixCol[A]]) extends Def[Int]

  //def matrixcol_index[A:Manifest](x: Exp[MatrixCol[A]]) = reflectPure(MatrixColIndex(x))
}

trait MatrixColOpsExpOpt extends MatrixColOpsExp { this: OptiLAExp =>

  // override def matrixcol_index[A:Manifest](x: Exp[MatrixCol[A]]) = x match {
  //     case Def(MatrixGetCol(m, j)) => j
  //     case _ => super.matrixcol_index(x)
  //   }
}

trait ScalaGenMatrixColOps extends ScalaGenBase {
  val IR: MatrixColOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //     // these are the ops that call through to the underlying real data structure
  //     case MatrixColIndex(x)   => emitValDef(sym, quote(x) + ".index")
  //     case _ => super.emitNode(sym, rhs)
  //   }
}