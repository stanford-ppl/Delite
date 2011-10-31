package ppl.dsl.optiml.stream

import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml.datastruct.scala.{StreamRow, StreamRowImpl}
import ppl.dsl.optiml.{OptiMLExp, OptiMLExpOpt, OptiML}

trait StreamRowOps extends DSLType with Base with OverloadHack { this: OptiML =>

  def infix_index[A](x: Rep[StreamRow[A]])(implicit mA: Manifest[A], o: Overloaded2) = streamrow_index(x)

  // class defs
  def streamrow_index[A:Manifest](x: Rep[StreamRow[A]]): Rep[Int]
}

trait StreamRowOpsExp extends StreamRowOps with BaseExp { this: OptiMLExp =>

  // implemented via method on real data structure
  case class StreamRowIndex[A:Manifest](x: Exp[StreamRow[A]]) extends Def[Int]

  def streamrow_index[A:Manifest](x: Exp[StreamRow[A]]) = reflectPure(StreamRowIndex(x))
}

trait StreamRowOpsExpOpt extends StreamRowOpsExp { this: OptiMLExpOpt =>

  override def streamrow_index[A:Manifest](x: Exp[StreamRow[A]]) = x match {
    case Def(StreamChunkRow(Def(/*Reflect(*/StreamObjectNew(numRows,numCols,chunkSize,func,isPure)/*,_,_)*/), i, offset)) => offset*chunkSize + i
    //case Def(StreamChunkRow(Def(StreamObjectNew(numRows,numCols,chunkSize,func,isPure)), i, offset)) => offset*chunkSize + i
    case Def(StreamChunkRowFusable(Def(/*Reflect(*/StreamObjectNew(numRows,numCols,chunkSize,func,isPure)/*,_,_)*/), i, offset)) => offset*chunkSize + i
    case _ => super.streamrow_index(x)
  }
}

trait ScalaGenStreamRowOps extends ScalaGenBase {
  val IR: StreamRowOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case StreamRowIndex(x)   => emitValDef(sym, quote(x) + ".index")
    case _ => super.emitNode(sym, rhs)
  }
}