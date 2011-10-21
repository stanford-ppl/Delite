package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

/* IndexVector is an IntVector whose elements represent indices (e.g., of another vector).
 * It is either backed by a discrete sequence of integers (e.g. 1,5,10) or a continouous RangeVector.
 *
 * IndexVectors can be used for scatter/gather operations.
 *
 * They also provide a vector construction operator { } that takes a function mapping an index to a value,
 * producing a new vector.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Dec 27, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class IndexVectorDenseImpl(len: Int) extends DenseVector[Int](len, true) with IndexVectorDense

class IndexVectorRangeImpl(_start: Int, _end: Int) extends RangeVector(_start, _end, 1, true) with IndexVectorRange
