package ppl.dsl.optiml.apps.gda

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object GDA extends DeliteApplication with OptiMLExp {

  def print_usage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    val x = MLInputReader.read(args(0))
    val y = MLInputReader.readVector(args(1)).toBoolean(a => if (a <= 0) false else true)

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

    // TODO: get unit out of client code
    var y_ones = unit(0.0); var y_zeros = unit(0.0)
    var mu0_num = Vector.zeros(n); var mu1_num = Vector.zeros(n);

    /* This loop calculates all of the needed statistics with a single pass
       through the data.  */
    for (i <- 0 until m){
      if (y(i) == false){
        y_zeros = y_zeros + 1
        mu0_num = mu0_num + x(i)
      }
      else{
        y_ones = y_ones + 1
        mu1_num = mu1_num + x(i)
      }
    }

    //println("y_zeros: " + y_zeros)
    //println("y_ones: " + y_ones)

    val phi = 1./m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    var sigma = Matrix.zeros(n,n)
    for (i <- 0 until m){
      if (y(i) == false){
        sigma = sigma + ((x(i)-mu0).trans).outer(x(i)-mu0)
      }
      else{
        sigma = sigma + ((x(i)-mu1).trans).outer(x(i)-mu1)
      }
    }

    print("GDA parameter calculation finished: ")
    println("  phi = " + phi)
    println("  mu0 = " ); mu0.pprint
    println("  mu1 = " ); mu1.pprint
    println("  sigma = "); sigma.pprint
    //sigma
    
  }
}
