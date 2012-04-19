package ppl.dsl.optiml.stream

import ppl.dsl.optiml.CudaGenDataStruct
import ppl.dsl.optiml.{Vector, Stream, StreamRow}
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.{VariablesExp, Variables, DSLOpsExp, CGenBase, CudaGenBase, ScalaGenBase}
import scala.reflect.SourceContext
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import ppl.dsl.optiml.{OptiMLExp, OptiML}

/**
 * Streams are Matrix-like, but are not Matrices. A Stream (slice) can be converted to a Vector or Matrix.
 * Streams only support bulk operations with other streams.
 */
trait StreamOps extends Variables {
  this: OptiML =>

  object Stream {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[A])(implicit ctx: SourceContext) = stream_obj_new(numRows, numCols, func)
  }

  implicit def repStreamToStreamOps[A:Manifest](x: Rep[Stream[A]]) = new streamOpsCls(x)
  implicit def varToStreamOps[A:Manifest](x: Var[Stream[A]]) = new streamOpsCls(readVar(x))

  class streamOpsCls[A:Manifest](x: Rep[Stream[A]]) {
    def numRows(implicit ctx: SourceContext) = stream_numrows(x)
    def numCols(implicit ctx: SourceContext) = stream_numcols(x)
    def isPure(implicit ctx: SourceContext) = stream_ispure(x)

    def rows = new streamRowOpsCls(x)
    def foreachRow(block: Rep[StreamRow[A]] => Rep[Unit])(implicit ctx: SourceContext) = stream_foreachrow(x, block)
  }

  // syntactic sugar
  class streamRowOpsCls[A:Manifest](x: Rep[Stream[A]]) {
    def foreach(block: Rep[StreamRow[A]] => Rep[Unit])(implicit ctx: SourceContext) = x.foreachRow(block)
  }

  // object defs
  def stream_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int], func: (Rep[Int],Rep[Int]) => Rep[A])(implicit ctx: SourceContext): Rep[Stream[A]]

  // class defs
  def stream_ispure[A:Manifest](x: Rep[Stream[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def stream_numrows[A:Manifest](x: Rep[Stream[A]])(implicit ctx: SourceContext): Rep[Int]
  def stream_numcols[A:Manifest](x: Rep[Stream[A]])(implicit ctx: SourceContext): Rep[Int]

  def stream_foreachrow[A:Manifest](x: Rep[Stream[A]], block: Rep[StreamRow[A]] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
}

trait StreamOpsExp extends StreamOps with VariablesExp {
  this: OptiMLExp with StreamImplOps =>

  // used for all operations
  //val chunkSize = 10000
  def chunkSize(numCols: Rep[Int]): Rep[Int] = unit(100000)/numCols + unit(1000) // heuristic, total buffer size is chunkSize x numCols

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class StreamObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], chunkSize: Exp[Int],
                                         func: Exp[(Int,Int) => A], isPure: Exp[Boolean]) extends Def[Stream[A]] {
    val mA = manifest[A]
  }
  case class StreamIsPure[A:Manifest](x: Exp[Stream[A]]) extends Def[Boolean]
  case class StreamNumRows[A:Manifest](x: Exp[Stream[A]]) extends Def[Int]
  case class StreamNumCols[A:Manifest](x: Exp[Stream[A]]) extends Def[Int]
  case class StreamChunkRow[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], offset: Exp[Int]) extends Def[StreamRow[A]]
  case class StreamRawElem[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int]) extends Def[A]
  case class StreamInitRow[A:Manifest](x: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) extends Def[Unit]

  ////////////////////////////////
  // implemented via delite ops

  case class StreamChunkElem[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(stream_chunk_elem_impl(x,idx,j)))

  case class StreamRowsIn[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(stream_rowsin_impl(x,offset)))

  case class StreamInitChunk[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(stream_init_chunk_impl(x,offset)))

  /*
  case class StreamInitAndForeachRow[A:Manifest](in: Exp[Vector[Int]], v: Sym[Int], x: Exp[Stream[A]], offset: Exp[Int],
                                                 block: Exp[StreamRow[A]] => Exp[Unit])
    extends DeliteOpForeach[Int,Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(List())

    val func = reifyEffects {
      // always initialize for now (must be pure)
      block(stream_init_and_chunk_row(x, v, offset))
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
  */
  case class StreamInitAndForeachRow[A:Manifest](size: Exp[Int], x: Exp[Stream[A]], offset: Exp[Int],
                                                 block: Exp[StreamRow[A]] => Exp[Unit])
    extends DeliteOpIndexedLoop {

    def func = i => 
      // always initialize for now (must be pure)
      block(stream_init_and_chunk_row(x, i, offset))
  }

  case class StreamForeachRow[A:Manifest](size: Exp[Int], x: Exp[Stream[A]], offset: Exp[Int],
                                          block: Exp[StreamRow[A]] => Exp[Unit], init: Exp[Unit])
    extends DeliteOpIndexedLoop {

    def func = i => block(stream_chunk_row(x, i, offset))
  }

  ////////////////////
  // object interface

  def stream_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], func: (Rep[Int],Rep[Int]) => Rep[A])(implicit ctx: SourceContext) = {
    val y = doLambda2(func)
    val isPure = y match {
      case Def(Lambda2(a,b,c,Block(Def(Reify(d,u,es))))) => false
      case _ => true
    }
    // Streams are only mutable from an implementation standpoint (they hold underlying state)
    reflectPure/*Mutable*/(StreamObjectNew[A](numRows, numCols, chunkSize(numCols), y, unit(isPure)))
  }

  ////////////////////
  // class interface

  def stream_ispure[A:Manifest](x: Rep[Stream[A]])(implicit ctx: SourceContext) = reflectPure(StreamIsPure(x))
  def stream_numrows[A:Manifest](x: Exp[Stream[A]])(implicit ctx: SourceContext) = reflectPure(StreamNumRows(x))
  def stream_numcols[A:Manifest](x: Exp[Stream[A]])(implicit ctx: SourceContext) = reflectPure(StreamNumCols(x))

  def stream_foreachrow[A:Manifest](x: Exp[Stream[A]], block: Exp[StreamRow[A]] => Exp[Unit])(implicit ctx: SourceContext) = {
    // we do not know at compile time how many streaming chunks are needed (therefore how many ops to submit)
    // so we submit a While loop, where each iteration of the while depends on the next, and let the runtime unroll it
    val numChunks = ceil(x.numRows / chunkSize(x.numCols).doubleValue()).AsInstanceOf[Int]
    val i = var_new(unit(0))
    while (i < numChunks) {
      val rowsToProcess = stream_rowsin(x, i)
      val in = (unit(0)::rowsToProcess)
      //val v = fresh[Int]

      // discuss: 2 kinds of streams (computation-backed and file-backed)
      // computation-backed assume pure, always initialized in parallel; file-backed are always initialized sequentially
      // if (x.isComputationBased) { ..
      if (x.isPure) {
        // fuse parallel initialization and foreach function
        //reflectEffect(StreamInitAndForeachRow(in, v, x, i, block))   // parallel // should use effect summary based on loop body
        reflectEffect(StreamInitAndForeachRow(rowsToProcess, x, i, block))
      }
      else {
        val init = stream_init_chunk(x, i)  // sequential
        //reflectEffect(StreamForeachRow(in, v, x, i, block, init)) // parallel // should use effect summary based on loop body
        reflectEffect(StreamForeachRow(rowsToProcess, x, i, block, init))
      }

      i += unit(1)
    }
  }

  ///////////////////
  // internal

  def stream_chunk_row[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], offset: Exp[Int]) = reflectPure(StreamChunkRow(x, idx, offset))
  def stream_chunk_row_mutable[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], offset: Exp[Int]) = reflectMutable(StreamChunkRow(x, idx, offset))
  def stream_init_chunk[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int]) = reflectEffect/*Write(x)*/(StreamInitChunk(x, offset))
  def stream_init_row[A:Manifest](x: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) = reflectEffect/*Write(x)*/(StreamInitRow(x, row, offset))
  def stream_init_and_chunk_row[A:Manifest](x: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) = { stream_init_row(x,row,offset); stream_chunk_row(x,row,offset) }
  def stream_rowsin[A:Manifest](x: Exp[Stream[A]], offset: Exp[Int]) = reflectPure(StreamRowsIn(x, offset))
  def stream_chunk_elem[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int], j: Exp[Int]) = reflectPure(StreamChunkElem(x, idx, j))
  def stream_raw_elem[A:Manifest](x: Exp[Stream[A]], idx: Exp[Int]) = reflectPure(StreamRawElem(x, idx))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@StreamRawElem(x, idx) => reflectPure(StreamRawElem(f(x),f(idx)))(mtype(manifest[A]), implicitly[SourceContext])
    case e@StreamChunkElem(x, idx, j) => reflectPure(new { override val original = Some(f,e) } with StreamChunkElem(f(x),f(idx),f(j)))(mtype(manifest[A]), implicitly[SourceContext])
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait StreamOpsExpOpt extends StreamOpsExp {
  this: OptiMLExp with StreamImplOps =>
  
  override def stream_ispure[A:Manifest](x: Rep[Stream[A]])(implicit ctx: SourceContext) = x match {
    case Def(/*Reflect(*/StreamObjectNew(numRows, numCols, chunkSize, func, isPure)/*,_,_)*/) => isPure
    case _ => super.stream_ispure(x)
  }
  
  override def stream_numrows[A:Manifest](x: Exp[Stream[A]])(implicit ctx: SourceContext) = x match {
    case Def(/*Reflect(*/StreamObjectNew(numRows, numCols, chunkSize, func, isPure)/*,_,_)*/) => numRows
    case _ => super.stream_numrows(x)
  }
  
  override def stream_numcols[A:Manifest](x: Exp[Stream[A]])(implicit ctx: SourceContext) = x match {
    case Def(/*Reflect(*/StreamObjectNew(numRows, numCols, chunkSize, func, isPure)/*,_,_)*/) => numCols
    case _ => super.stream_numcols(x)
  }

  def stream_chunksize[A:Manifest](x: Exp[Stream[A]]) = x match {
    case Def(/*Reflect(*/StreamObjectNew(numRows, numCols, chunkSize, func, isPure)/*,_,_)*/) => chunkSize
  }

  def stream_stfunc[A:Manifest](x: Exp[Stream[A]]) = x match {
    case Def(/*Reflect(*/StreamObjectNew(numRows, numCols, chunkSize, Def(Lambda2(stfunc,_,_,_)), Const(true))/*,_,_)*/) => stfunc
  }
    
/*
  case class StreamRowFusable[A:Manifest](st: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) extends DeliteOpLoop[StreamRow[A]] {
    val size = in.length
    val v = fresh[Int]
    val body: Def[Stream[A]] = new DeliteCollectElem[A,Stream[A]](
      alloc = reifyEffects(stream_chunk_row(st,row,offset)),
      func = reifyEffects(v),
    )
  }
*/

  /*case class StreamChunkRowFusable[A:Manifest](st: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int])
    extends DeliteOpMap[Int,A,StreamRow[A]] {

    val size = stream_numcols(st)
    val in = 0::size
    
    val chunkSize = stream_chunksize(st)
    val stfunc = stream_stfunc(st)
    
    def alloc = VectorNew[A](size, unit(true))//stream_chunk_row(st,row,offset) // FIXME: not supported right now
    def func = i => stfunc(offset*chunkSize+row,i)
  }*/
  
  // no unsafeSetData exists for views... so we have to unfortunately do an extra copy to wrap the array result (i.e. safeSetData)
  //def updateViewWithArray[A:Manifest](a: Exp[Array[A]], v: Exp[StreamRow[A]]): Exp[StreamRow[A]] = { vectorview_update_impl(v, a); v }
  
  abstract case class StreamChunkRowFusable[A:Manifest](st: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]) extends DeliteOpLoop[StreamRow[A]]

  override def stream_init_and_chunk_row[A:Manifest](st: Exp[Stream[A]], row: Exp[Int], offset: Exp[Int]): Exp[StreamRow[A]] = st match {

    case Def(/*Reflect(*/StreamObjectNew(numRows, numCols, chunkSize, Def(Lambda2(stfunc,_,_,_)), Const(true))/*,_,_)*/) =>
/*
      // initRow
        var j = 0
        while (j < numCols) {
          _data(row*numCols+j) = stfunc(offset*chunkSize+row,j)
          j += 1
        }
      }
      // chunkRow
      //vview(idx*numCols, 1, numCols, true)
      new StreamRowImpl[T](idx, offset, this, _data)
*/
      
      printerr("warning: fusable chunked stream rows are currently not supported FIXME")
      
      return super.stream_init_and_chunk_row(st,row,offset)
      
      val r: Def[StreamRow[A]] = new StreamChunkRowFusable(st, row, offset) {
        val size = numCols
        val aV = fresh[Array[A]]
        val body: Def[StreamRow[A]] = new DeliteCollectElem[A,StreamRow[A]](
          //aV = this.aV,
          //alloc = reifyEffects(updateViewWithArray(aV,stream_chunk_row_mutable(st,row,offset))),
          aV = fresh[Array[A]],
          alloc = reifyEffects(stream_chunk_row(st,row,offset)), // <--- will ignore the actual data array. stream rows do not have unsafeSetData
          func = reifyEffects(stfunc(offset*chunkSize+row,v))
        )
      }
      r
      
    case _ => super.stream_init_and_chunk_row(st,row,offset)
  }
  
}



trait ScalaGenStreamOps extends ScalaGenBase {
  val IR: StreamOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case m@StreamObjectNew(numRows, numCols, chunkSize, func, isPure) =>
      emitValDef(sym, "new " + remap("generated.scala.Stream[" + remap(m.mA) + "]")+"(" + quote(numRows) + "," + quote(numCols) + "," + quote(chunkSize) + ","
                      + quote(func) + "," + quote(isPure) + ")")
    case StreamIsPure(x) => emitValDef(sym, quote(x) + ".isPure")
    case StreamNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case StreamNumCols(x)  => emitValDef(sym, quote(x) + ".numCols")
    case StreamChunkRow(x, idx, offset) => emitValDef(sym, quote(x) + ".chunkRow(" + quote(idx) + "," + quote(offset) + ")")
    case StreamRawElem(x, idx) => emitValDef(sym, quote(x) + ".rawElem(" + quote(idx) + ")")
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












