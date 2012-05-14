package ppl.apps.ml.gda

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object GDARunnerNCNR extends OptiMLApplicationRunnerBase with OptiMLNoCSE with OptiMLExp with GDA
object GDARunnerNC extends OptiMLApplicationRunner with OptiMLNoCSE with GDA
object GDARunnerNR extends OptiMLApplicationRunnerBase with OptiMLExp with GDA
object GDARunner extends OptiMLApplicationRunner with GDA

trait GDA extends OptiMLApplication {
  def printUsage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage
    
    val x = readMatrix(args(0))
    val y = readVector(args(1)).toBoolean(a => if (a <= 0) false else true)
    tic()

    /* number of training samples */
    val m = y.length
    //val m = 2

    /* dimensionality of training data */
    val n = x.numCols

    /* Number of training points must equal number of labeled output points */
    //if (x.h != m) throw new RuntimeException("Bad inputs to GDA")

    /* phi, mu0, mu1, and sigma parameterize the GDA model, where we assume the
     * input features are continuous-valued random variables with a multivariate
     * normal distribution.
     *
     * phi is a scalar, mu0 and mu1 are n dimensional vectors,
     * where n is the width of x, and sigma is an n x n matrix.
     */

    val y_zeros = y count { _ == false } 
    val y_ones = y count { _ == true }
    val mu0_num = sumIf[DenseVector[Double],VectorView[Double]](0,m) { !y(_) } { x(_) } 
    val mu1_num = sumIf[DenseVector[Double],VectorView[Double]](0,m) { y(_) } { x(_) } 
    
    //println("y_zeros: " + y_zeros)
    //println("y_ones: " + y_ones)

    val phi = 1./m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    val sigma = sum(0, m) { i =>
      if (y(i) == false){
        (((x(i)-mu0).t) ** (x(i)-mu0))
      }
      else{
        (((x(i)-mu1).t) ** (x(i)-mu1))
      }
    }

    toc(sigma)

    //print("GDA parameter calculation finished: ")
    //println("  phi = " + phi)
    //println("  mu0 = " ); mu0.pprint
    //println("  mu1 = " ); mu1.pprint
    //println("  sigma = "); sigma.pprint

    // need to do something or the whole program will be DCE'd
    println(sigma)
  }
}
