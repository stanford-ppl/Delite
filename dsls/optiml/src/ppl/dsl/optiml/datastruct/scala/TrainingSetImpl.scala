package ppl.dsl.optiml.datastruct.scala

class TrainingSetImpl[T:Manifest,L:Manifest](xs: Matrix[T], __labels: Labels[L]) extends MatrixImpl[T](0,0) with TrainingSet[T,L] {
  var _labels: Labels[L] = __labels
  lazy val transposed: TrainingSet[T,L] = transpose()

  def labels = _labels

  def transpose() = {
    val out = new MatrixImpl[T](numFeatures, numSamples)
    for (i <- 0 until numSamples) {
      for (j <- 0 until numFeatures) {
        out(j,i) = this(i,j)
      }
    }
    new TrainingSetImpl[T,L](out, _labels)
  }

  // not a deep copy, use with care
  _data = xs.data
  _numRows = xs.numRows
  _numCols = xs.numCols

}