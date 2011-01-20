package ppl.apps.ml.gda

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

    tic

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

    /* This loop calculates all of the needed statistics with a single pass
       through the data.  */

    // the major problem here is the sum function is expanded statically, including instantiation and looping over vectors,
    // but the conditional means that whether or not we are adding with a zerovector is only known dynamically
    // -- the issue MUST be resolved in the generated code, via dynamic checks.. but how?
    // + will generate a ZipWith kernel with 2 vectors, one of which may be a Zero
    // perhaps the preamble to ZipWith is a test for Zero and shortcircuit - screws up cost predictions though
    // Zero generates a references to a static object ZeroVector which is dynamically matched against in the generated code
    // on the other hand, is a dynamic check for a zero vector on every += worth it?
    // (current solution): an alternative is to implement ZeroVector apply
    // to always return 0 -- this is space efficient, but a += will still be time inefficient (n additions of zero).. jit magic to the rescue?
    val (y_zeros, y_ones, mu0_num, mu1_num) = t4( sum(0,m) { i =>
      if (y(i) == false){
        (unit(1.),unit(0.),x(i),NilV[Double])
      }
      else {
        (unit(0.),unit(1.),NilV[Double],x(i))
      }
    })

    //println("y_zeros: " + y_zeros)
    //println("y_ones: " + y_ones)

    val phi = 1./m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    val sigma = sum(0, m) { i =>
      if (y(i) == false){
       (((x(i)-mu0).t)**(x(i)-mu0))
      }
      else{
       (((x(i)-mu1).t)**(x(i)-mu1))
      }
    }

    // TODO: nothing is really preventing toc from getting hoisted upwards in the schedule, resulting in the wrong time
    // unless the above sum is a reflectEffect
    toc

    //print("GDA parameter calculation finished: ")
    //println("  phi = " + phi)
    //println("  mu0 = " ); mu0.pprint
    //println("  mu1 = " ); mu1.pprint
    //println("  sigma = "); sigma.pprint
    println(sigma) //TR: not lifted??
    sigma.pprint

  }
}
