package ppl.dsl.tests.GPU_tests

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication


object SVM_kernelsRunner extends OptiMLApplicationRunner with SVM_kernels

trait SVM_kernels extends OptiMLApplication {

  def main() {
    println("SVM kernel generation")

    val alphas = Vector[Double](10)
    val Y = Vector[Double](10)
    val X = Matrix[Double](10,10)
    var b = 0.0
    val i = 5
    
    val f_i = (alphas*Y*(X*X(i).t)).sum + b
    println("f_i = " + f_i)
  }
}
