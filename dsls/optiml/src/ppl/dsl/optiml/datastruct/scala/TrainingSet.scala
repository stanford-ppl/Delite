package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

/*
class TrainingSetImpl[T:Manifest,L:Manifest](xs: Matrix[T], var _labels: Labels[L], trans: TrainingSetImpl[T,L] = null) extends DenseMatrix[T](0,0) with TrainingSet[T,L] {

  // not a deep copy, use with care
  _data = xs.data
  _numRows = xs.numRows
  _numCols = xs.numCols

  val transposed = if (trans != null) trans else transpose

  def labels = _labels

  private def transpose = {
    val out = new DenseMatrix[T](numFeatures, numSamples)
    for (i <- 0 until numSamples) {
      for (j <- 0 until numFeatures) {
        out(j,i) = this(i,j)
      }
    }
    new TrainingSetImpl[T,L](out, _labels, this)
  }

}
*/

class SupervisedTrainingSet[T:Manifest,L:Manifest](val _data: DenseMatrix[T], val _labels: DenseVector[L])
class UnsupervisedTrainingSet[T:Manifest](val _data: DenseMatrix[T])