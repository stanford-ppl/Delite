package ppl.dsl.optiml

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala.{Vector, Stream, StreamImpl}
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{VariablesExp, Variables, DSLOpsExp, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config

/**
 * Streams are Matrix-like, but are not Matrices. A Stream (slice) can be converted to a Vector or Matrix.
 * Streams only support bulk operations with other streams.
 */
trait StreamOps extends DSLType with Variables {
  this: OptiML =>

  object Stream {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[A]) = stream_obj_new(numRows, numCols, func)
  }

  implicit def repStreamToStreamOps[A:Manifest](x: Rep[Stream[A]]) = new streamOpsCls(x)
  implicit def varToStreamOps[A:Manifest](x: Var[Stream[A]]) = new streamOpsCls(readVar(x))

  class streamOpsCls[A:Manifest](x: Rep[Stream[A]]) {
    def numRows = stream_numrows(x)
    def numCols = stream_numcols(x)

    def foreachRow(block: Rep[Vector[A]] => Rep[Unit]) = stream_foreachrow(x, block)
  }

  // object defs
  def stream_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int], func: (Rep[Int],Rep[Int]) => Rep[A]): Rep[Stream[A]]

  // class defs
  def stream_numrows[A:Manifest](x: Rep[Stream[A]]): Rep[Int]
  def stream_numcols[A:Manifest](x: Rep[Stream[A]]): Rep[Int]

  def stream_foreachrow[A:Manifest](x: Rep[Stream[A]], block: Rep[Vector[A]] => Rep[Unit]): Rep[Unit]
}

trait StreamOpsExp extends StreamOps with VariablesExp {
  this: OptiMLExp  =>

  // used for all operations
  val chunkSize = 10000

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class StreamObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], chunkSize: Exp[Int], func: Rep[(Int,Int) => A]) extends Def[Stream[A]] {
    val mI = manifest[StreamImpl[A]]
  }
  case class StreamNumRows[A:Manifest](x: Exp[Stream[A]]) extends Def[Int]
  case class StreamNumCols[A:Manifest](x: Exp[Stream[A]]) extends Def[Int]
  case class StreamChunkRow[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int]) extends Def[Vector[A]]

  case class StreamInit[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int]) extends Def[Unit]

  ////////////////////////////////
  // implemented via delite ops

  case class StreamForeachRow[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int], v: Sym[Vector[A]], func: Exp[Unit], init: Exp[Unit])
    extends DeliteOpForeach[Vector[A],Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(List())
    val in = reifyEffects {
      val remainingRows = x.numRows - offset*chunkSize
      val leftover = if (remainingRows < 0) x.numRows else remainingRows
      val size = Math.min(chunkSize, leftover).asInstanceOfL[Int]
      var tcoll = Vector[Vector[A]](size, unit(false))
       for (i <- 0 until size){
         tcoll(i) = stream_chunkrow(x, i)
       }
      tcoll
    }
  }

  ////////////////////
  // object interface

  def stream_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], func: (Rep[Int],Rep[Int]) => Rep[A])
    = reflectEffect(StreamObjectNew[A](numRows, numCols, chunkSize, doLambda2(func)))

  ////////////////////
  // class interface

  def stream_numrows[A:Manifest](x: Exp[Stream[A]]) = reflectPure(StreamNumRows(x))
  def stream_numcols[A:Manifest](x: Exp[Stream[A]]) = reflectPure(StreamNumCols(x))

  def stream_foreachrow[A:Manifest](x: Exp[Stream[A]], block: Exp[Vector[A]] => Exp[Unit]) = {
    val v = fresh[Vector[A]]
    val func = reifyEffects(block(v))

    // we do not know at compile time how many streaming chunks are needed (therefore how many ops to submit)
    // so we submit a While loop, where each iteration of the while depends on the next, and let the runtime unroll it
    val numChunks = Math.ceil(x.numRows / unit(chunkSize).doubleValue()).asInstanceOfL[Int]
    val i = var_new(0)
    while (i < numChunks) {
      val init = reflectWrite(x)(StreamInit(x, i))
      reflectEffect(StreamForeachRow(x, i, v, func, init)) // read??
      i += 1
    }
  }

  ///////////////////
  // internal

  def stream_chunkrow[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int]) = reflectPure(StreamChunkRow(x, idx))
}


trait ScalaGenStreamOps extends ScalaGenBase {
  val IR: StreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case m@StreamObjectNew(numRows, numCols, chunkSize, func) => emitValDef(sym, "new " + remap(m.mI) + "(" + quote(numRows) + ","
                                                                       + quote(numCols) + "," + quote(chunkSize) + "," + quote(func) + ")")
    case StreamNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case StreamNumCols(x)  => emitValDef(sym, quote(x) + ".numCols")
    case StreamChunkRow(x, idx) => emitValDef(sym, quote(x) + ".chunkRow(" + quote(idx) + ")")
    case StreamInit(x, idx) => emitValDef(sym, quote(x) + ".init(" + quote(idx) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStreamOps extends CudaGenBase with CudaGenDataStruct {
  val IR: StreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenStreamOps extends CGenBase {
  val IR: StreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}












