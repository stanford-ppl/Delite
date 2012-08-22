package ppl.dsl.optiml.stream

import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase}
import scala.reflect.SourceContext
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optila.vector.{DenseVectorViewOpsExp}
import ppl.dsl.optiml._

trait StreamRowOps extends Base with OverloadHack { this: OptiML =>

  def infix_index[A](x: Rep[StreamRow[A]])(implicit mA: Manifest[A], o: Overloaded2, ctx: SourceContext) = streamrow_index(x)

  // class defs
  def streamrow_index[A:Manifest](x: Rep[StreamRow[A]])(implicit ctx: SourceContext): Rep[Int]
}

trait StreamRowOpsExp extends StreamRowOps with BaseExp { this: OptiMLExp =>

  // implemented via method on real data structure
  case class StreamRowIndex[A:Manifest](x: Exp[StreamRow[A]]) extends Def[Int]

  def streamrow_index[A:Manifest](x: Exp[StreamRow[A]])(implicit ctx: SourceContext): Rep[Int] 
}

trait StreamRowOpsExpOpt extends StreamRowOpsExp with DenseVectorViewOpsExp { this: OptiMLExp =>

  override def dense_vectorview_length[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(StreamChunkRow(s, idx, off)) => s.numCols
    case Def(v@Reflect(StreamChunkRow(s,idx,off), u, es)) /*if context.contains(v)*/ => s.numCols 
    case _ => super.dense_vectorview_length(x) 
  }  
  
  override def dense_vectorview_isrow[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(StreamChunkRow(s, idx, off)) => Const(true)
    case Def(v@Reflect(StreamChunkRow(s,idx,off), u, es)) /*if context.contains(v)*/ => Const(true)
    case _ => super.dense_vectorview_isrow(x) //throw new RuntimeException("could not resolve type of " + findDefinition(x.asInstanceOf[Sym[DenseVectorView[A]]]).get.rhs) 
  }
  
  override def streamrow_index[A:Manifest](x: Exp[StreamRow[A]])(implicit ctx: SourceContext) = x match {
    case Def(StreamChunkRow(Def(/*Reflect(*/StreamObjectNew(numRows,numCols,chunkSize,func,isPure)/*,_,_)*/), i, offset)) => offset*chunkSize + i
    //case Def(StreamChunkRow(Def(StreamObjectNew(numRows,numCols,chunkSize,func,isPure)), i, offset)) => offset*chunkSize + i
    case Def(StreamChunkRowFusable(Def(/*Reflect(*/StreamObjectNew(numRows,numCols,chunkSize,func,isPure)/*,_,_)*/), i, offset)) => offset*chunkSize + i
    case _ => err("internal error: streamrow_index should have been overridden") //super.streamrow_index(x)
  }
}

trait ScalaGenStreamRowOps extends ScalaGenBase {
  val IR: StreamRowOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //     // these are the ops that call through to the underlying real data structure
  //     case StreamRowIndex(x)   => emitValDef(sym, quote(x) + ".index")
  //     case _ => super.emitNode(sym, rhs)
  //   }
}