/* locally weighted linear regression
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.apps.ml.linreg

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object LinRegRunner extends OptiMLApplicationRunner with LinReg

trait LinReg extends OptiMLApplication {

  // unweighted linear regression using the normal equations
  // input: input training vector x
  //        output training vector y
  // output: predictions along uniformly sampled points
  def unweightedReg(x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] =
  {
    // by convention, x_0 = 1
    val X = Matrix(Vector.ones(x.length).t, x)

    // theta = inv(X.'X)*(X.'*y) (if y is a col vector)
    val theta = ((X.t*X).inv)*(X.t*y)

    // the resulting fitted line is given by the equation
    //   h(x) = theta_0 + theta_1*x_1 + ...
    theta
  }

  def weightedReg(x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    val tau = 10
    val X = Matrix(Vector.ones(x.length).t, x)

    // initialize prediction points
    val xstep = 25.0/X.numRows
    val xref_pts = Vector.uniform(-10, xstep, 14.99).t
    val xref = Matrix(Vector.ones(xref_pts.length).t, xref_pts)
    //val O = Matrix.identity(X.numRows)
    val Xt = X.t

    // calculate predictions
    val guess = (0::xref.numRows){ e =>
      val x_cur = xref(e,1)
      val weights = x.map(ele => Math.exp(-.1*(x_cur-ele)*(x_cur-ele)/(2.0*tau*tau))/2.0)
      val W = Matrix.diag(weights.length, weights)
      val t1 = Xt*W
      val theta = ((t1*X).inv)*(t1*y)
      (theta.t) *:* (xref(e).t)
    }

    guess
  }

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LinRegSerial <input vector file> <output vector file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    val x = readVector(args(0)).t
    val y = readVector(args(1)).t

//    logElapsed("Input Section Complete")

    val theta = unweightedReg(x, y)
    println("Unweighted linear regression")
    println("theta: ")
    theta.pprint
    print("\\n")

    tic()
    val guess = weightedReg(x, y)
    toc(guess)

    println("Locally weighted linear regression")
    println("guess: ")
    guess.pprint
    print("\\n")

    //PerformanceTimer.save("LinReg")
  }
}
