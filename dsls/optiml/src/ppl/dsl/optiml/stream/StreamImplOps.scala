package ppl.dsl.optiml.stream

import ppl.dsl.optiml.{Vector, Matrix, Stream}
import ppl.dsl.optiml.{OptiMLLift, OptiMLCompiler, OptiML}

trait StreamImplOps { this: OptiML =>
  def stream_chunk_elem_impl[A:Manifest](x: Rep[Stream[A]], idx: Rep[Int], j: Rep[Int]): Rep[A]
  def stream_rowsin_impl[A:Manifest](x: Rep[Stream[A]], offset: Rep[Int]): Rep[Int]
  def stream_init_chunk_impl[A:Manifest](x: Rep[Stream[A]], offset: Rep[Int]): Rep[Unit]
}

trait StreamImplOpsStandard extends StreamImplOps {
  this: OptiMLCompiler with OptiMLLift with StreamOpsExp =>

  def stream_chunk_elem_impl[A:Manifest](x: Rep[Stream[A]], idx: Rep[Int], j: Rep[Int]) = {
    val offset = idx*x.numCols+j
    stream_raw_elem(x, offset)
  }

  def stream_rowsin_impl[A:Manifest](x: Rep[Stream[A]], offset: Rep[Int]) = {
    val chunkSz = chunkSize(x.numCols)
    val remainingRows = x.numRows - offset*chunkSz
    val leftover = if (remainingRows < 0) x.numRows else remainingRows // in case numRows < chunkSize
    min(chunkSz, leftover).AsInstanceOf[Int]
  }

  def stream_init_chunk_impl[A:Manifest](x: Rep[Stream[A]], offset: Rep[Int]) = {
    val numRows = stream_rowsin(x,offset)
    var i = 0
    while (i < numRows) {
      stream_init_row(x, i, offset)
      i += 1
    }
  }
}
