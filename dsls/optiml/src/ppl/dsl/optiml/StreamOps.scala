package ppl.dsl.optiml

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala.{Vector, Stream, StreamImpl, StreamRow}
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
    def isPure = stream_ispure(x)

    def foreachRow(block: Rep[StreamRow[A]] => Rep[Unit]) = stream_foreachrow(x, block)
  }

  // object defs
  def stream_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int], func: (Rep[Int],Rep[Int]) => Rep[A]): Rep[Stream[A]]

  // class defs
  def stream_ispure[A:Manifest](x: Rep[Stream[A]]): Rep[Boolean]
  def stream_numrows[A:Manifest](x: Rep[Stream[A]]): Rep[Int]
  def stream_numcols[A:Manifest](x: Rep[Stream[A]]): Rep[Int]

  def stream_foreachrow[A:Manifest](x: Rep[Stream[A]], block: Rep[StreamRow[A]] => Rep[Unit]): Rep[Unit]
}

trait StreamOpsExp extends StreamOps with VariablesExp {
  this: OptiMLExp => //with StreamImplOps =>

  // used for all operations
  val chunkSize = 10000

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class StreamObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], chunkSize: Exp[Int],
                                         func: Exp[(Int,Int) => A], isPure: Exp[Boolean]) extends Def[Stream[A]] {
    val mI = manifest[StreamImpl[A]]
  }
  case class StreamIsPure[A:Manifest](x: Exp[Stream[A]]) extends Def[Boolean]
  case class StreamNumRows[A:Manifest](x: Exp[Stream[A]]) extends Def[Int]
  case class StreamNumCols[A:Manifest](x: Exp[Stream[A]]) extends Def[Int]
  case class StreamChunkRow[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], offset: Exp[Int]) extends Def[StreamRow[A]]
  case class StreamChunkElem[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], j: Exp[Int]) extends Def[A]
  case class StreamRowsIn[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int]) extends Def[Int]
  case class StreamInitChunk[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int]) extends Def[Unit]
  case class StreamInitRow[A:Manifest](x: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) extends Def[Unit]

  ////////////////////////////////
  // implemented via delite ops

  case class StreamInitAndForeachRow[A:Manifest](in: Exp[Vector[Int]], v: Sym[Int], x: Exp[Stream[A]], offset: Exp[Int],
                                                 block: Exp[StreamRow[A]] => Exp[Unit])
    extends DeliteOpForeach[Int,Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(List())

    val func = reifyEffects {
      // always initialize for now (must be pure)
      stream_init_row(x, v, offset)
      block(stream_chunk_row(x, v, offset))
    }
  }

  case class StreamForeachRow[A:Manifest](in: Exp[Vector[Int]], v: Sym[Int], x: Exp[Stream[A]], offset: Exp[Int],
                                          block: Exp[StreamRow[A]] => Exp[Unit], init: Exp[Unit])
    extends DeliteOpForeach[Int,Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(List())

    val func = reifyEffects {
      block(stream_chunk_row(x, v, offset))
    }
  }

  ////////////////////
  // object interface

  def stream_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], func: (Rep[Int],Rep[Int]) => Rep[A]) = {
    val y = doLambda2(func)
    val isPure = y match {
      case Def(Lambda2(a,b,c,Def(Reify(d,u,es)))) => false
      case _ => true
    }
    reflectEffect(StreamObjectNew[A](numRows, numCols, chunkSize, y, unit(isPure)))
  }

  ////////////////////
  // class interface

  def stream_ispure[A:Manifest](x: Rep[Stream[A]]) = reflectPure(StreamIsPure(x))
  def stream_numrows[A:Manifest](x: Exp[Stream[A]]) = reflectPure(StreamNumRows(x))
  def stream_numcols[A:Manifest](x: Exp[Stream[A]]) = reflectPure(StreamNumCols(x))

  def stream_foreachrow[A:Manifest](x: Exp[Stream[A]], block: Exp[StreamRow[A]] => Exp[Unit]) = {
    // we do not know at compile time how many streaming chunks are needed (therefore how many ops to submit)
    // so we submit a While loop, where each iteration of the while depends on the next, and let the runtime unroll it
    val numChunks = Math.ceil(x.numRows / unit(chunkSize).doubleValue()).asInstanceOfL[Int]
    val i = var_new(0)
    while (i < numChunks) {
      val rowsToProcess = stream_rowsin(x, i)
      val in = (0::rowsToProcess)
      val v = fresh[Int]

      // discuss: 2 kinds of streams (computation-backed and file-backed)
      // computation-backed assume pure, always initialized in parallel; file-backed are always initialized sequentially
      // if (x.isComputationBased) { ..
      if (x.isPure) {
        // fuse parallel initialization and foreach function
        reflectWrite(x)(StreamInitAndForeachRow(in, v, x, i, block))   // parallel
      }
      else {
        val init = stream_init_chunk(x, i)  // sequential
        reflectEffect(StreamForeachRow(in, v, x, i, block, init))
      }

      i += 1
    }
  }

  ///////////////////
  // internal

  def stream_chunk_row[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], offset: Exp[Int]) = reflectPure(StreamChunkRow(x, idx, offset))
  def stream_init_chunk[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int]) = reflectWrite(x)(StreamInitChunk(x, offset))
  def stream_init_row[A:Manifest](x: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) = reflectWrite(x)(StreamInitRow(x, row, offset))
  def stream_rowsin[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int]) = reflectPure(StreamRowsIn(x, offset))
  def stream_chunk_elem[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], j: Exp[Int]) = reflectPure(StreamChunkElem(x, idx, j))
}


trait ScalaGenStreamOps extends ScalaGenBase {
  val IR: StreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case m@StreamObjectNew(numRows, numCols, chunkSize, func, isPure) =>
      emitValDef(sym, "new " + remap(m.mI) + "(" + quote(numRows) + "," + quote(numCols) + "," + quote(chunkSize) + ","
                      + quote(func) + "," + quote(isPure) + ")")
    case StreamIsPure(x) => emitValDef(sym, quote(x) + ".isPure")
    case StreamNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case StreamNumCols(x)  => emitValDef(sym, quote(x) + ".numCols")
    case StreamChunkRow(x, idx, offset) => emitValDef(sym, quote(x) + ".chunkRow(" + quote(idx) + "," + quote(offset) + ")")
    case StreamChunkElem(x, idx, j) => emitValDef(sym, quote(x) + ".chunkElem(" + quote(idx) + "," + quote(j) + ")")
    case StreamRowsIn(x, idx) => emitValDef(sym, quote(x) + ".rowsIn(" + quote(idx) + ")")
    case StreamInitChunk(x, idx) => emitValDef(sym, quote(x) + ".initChunk(" + quote(idx) + ")")
    case StreamInitRow(x, row, idx) => emitValDef(sym, quote(x) + ".initRow(" + quote(row) + "," + quote(idx) + ")")
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












