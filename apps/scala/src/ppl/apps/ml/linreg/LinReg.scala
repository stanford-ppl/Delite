/* locally weighted linear regression
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 1/2/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.apps.ml.linreg

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object LinRegRunner extends OptiMLApplicationRunner with LinReg

trait LinReg extends OptiMLApplication {

  // unweighted linear regression using the normal equations
  // input: input training vector x
  //        output training vector y
  // output: predictions along uniformly sampled points
  def unweightedReg(x: Rep[Vector[Double]], y: Rep[Vector[Double]]) : Rep[Vector[Double]] =
  {
    // by convention, x_0 = 1
    // TODO: the nice syntax doesn't work because of our problems with instantiating Vectors from sequences
    //val X = Matrix(x.map(ele => Vector(1., ele)))
    // could be (probably should be) written as an insertCol
    val X = Matrix[Double](x map {ele => val v = Vector[Double](2, true); v(0) = 1.; v(1) = ele; v})

    // theta = inv(X.'X)*(X.'*y) (if y is a col vector)
    val theta = ((X.t*X).inv)*(X.t*y.t)

    // the resulting fitted line is given by the equation
    //   h(x) = theta_0 + theta_1*x_1 + ...
    return theta
  }

  def weightedReg(x: Rep[Vector[Double]], y: Rep[Vector[Double]]) : Rep[Vector[Double]] = {
    val tau = 10
    //val X = Matrix(x.map(ele => Vector(1., ele)))
    val X = Matrix[Double](x map {ele => val v = Vector[Double](2, true); v(0) = 1.; v(1) = ele; v})

    // initialize prediction points
    val xstep = 25.0/X.numRows
    val xref_pts = Vector.uniform(-10, xstep, 14.99).t
    //val xref = Matrix(xref_pts.map(ele => Vector(1., ele)))
    val xref = Matrix[Double](xref_pts map {ele => val v = Vector[Double](2, true); v(0) = 1.; v(1) = ele; v})
    //val O = Matrix.identity(X.numRows)
    val Xt = X.t

    // calculate predictions
    val guess = (0::xref.numRows)( e => {
      val x_cur = xref(e,1)
      val weights = x.map(ele => Math.exp(-.1*(x_cur-ele)*(x_cur-ele)/(2.0*tau*tau))/2.0)
      val W = Matrix.diag(weights.length, weights)
      val t1 = Xt*W
      val theta = ((t1*X).inv)*(t1*y) // relaxed v_prod, ignore is_row on y
      (theta.t) *:* (xref(e).t)
    })

    return guess
  }

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LinRegSerial <input vector file> <output vector file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    val x = readVector(args(0))
    val y = readVector(args(1))

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
