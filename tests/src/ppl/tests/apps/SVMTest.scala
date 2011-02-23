package ppl.tests.apps

import scala.virtualization.lms.internal.ScalaCompile

object TestGenSVM {
  def main(args: Array[String]) {
    val a = Array("data/ml/svm/MATRIX.TRAIN.50", "data/ml/svm/MATRIX.TEST")

    //this just generates
    import ppl.apps.ml.svm.SVMRunner
    SVMRunner.main(a)
  }
}