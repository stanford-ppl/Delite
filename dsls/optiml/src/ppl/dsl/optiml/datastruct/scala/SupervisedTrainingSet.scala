package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class SupervisedTrainingSet[T:Manifest,L:Manifest](val _data: DenseMatrix[T], val _labels: DenseVector[L])