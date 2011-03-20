package ppl.dsl.optiml

import datastruct.scala.{Vector, Matrix, Stream}

trait StreamImplOps { this: OptiMLExp =>
  def stream_chunk_elem_impl[A:Manifest](x: Rep[Stream[A]], idx: Rep[Int], j: Rep[Int]): Rep[A]
}

trait StreamImplOpsStandard extends StreamImplOps {
  this: OptiMLExp with OptiMLLift =>

  def stream_chunk_elem_impl[A:Manifest](x: Rep[Stream[A]], idx: Rep[Int], j: Rep[Int]) = {
    val offset = idx*x.numCols+j
    stream_raw_elem(x, offset)
  }
}
