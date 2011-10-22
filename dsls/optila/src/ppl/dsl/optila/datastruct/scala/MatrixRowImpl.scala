package ppl.dsl.optila.datastruct.scala

/* MatrixRowImpl wraps a VectorView that represents a Matrix row.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 3/15/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

// class MatrixRowImpl[T:Manifest](val index: Int, matrix: Matrix[T], x: Array[T])
//   extends VectorViewImpl[T](x, index*matrix.numCols, 1, matrix.numCols, true) with MatrixRow[T]