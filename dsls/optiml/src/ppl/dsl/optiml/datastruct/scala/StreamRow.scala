package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

/* StreamRowImpl wraps a VectorView that represents a Stream row.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 3/15/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class StreamRow[T:Manifest](chunkRow: Int, offset: Int, stream: Stream[T], x: Array[T])
  extends VectorView[T](x, chunkRow*stream.numCols, 1, stream.numCols, true) {

  // absolute row index in the stream
  val index = offset*stream.chunkSize + chunkRow
}