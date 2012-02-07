package ppl.apps.ml.gda

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object GDAVectorizedRunner extends OptiMLApplicationRunner with GDAVectorized

trait GDAVectorized extends OptiMLApplication {

  def print_usage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    val x = readMatrix(args(0))
    val y = readVector(args(1)).toBoolean(a => if (a <= 0) false else true)

    tic()

    /* number of training samples */
    val m = y.length
    //val m = 2

    /* dimensionality of training data */
    val n = x.numCols

    val (y_zeros, y_ones, mu0_num, mu1_num) = t4( sum(0,m) { i =>
      if (y(i) == false){
        (unit(1.),unit(0.),x(i).Clone,EmptyVector[Double])
      }
      else {
        (unit(0.),unit(1.),EmptyVector[Double],x(i).Clone)
      }
    })

    //println("y_zeros: " + y_zeros)
    //println("y_ones: " + y_ones)

    val phi = 1./m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    val x0 = Matrix((0::x.numRows) map { i => if (y(i) == true) Vector.zeros(x.numCols) else x(i).Clone })
    val x0t = x0 filterRows { row => !(row.sum == 0.0) }
    val x1 = Matrix((0::x.numRows) map { i => if (y(i) == false) Vector.zeros(x.numCols) else x(i).Clone })
    val x1t = x1 filterRows { row => !(row.sum == 0.0) }

    val x0tt = x0t - mu0.replicate(y_zeros.AsInstanceOf[Int], 1)
    val x1tt = x1t - mu1.replicate(y_ones.AsInstanceOf[Int], 1)

    val sigma = x0tt.t*x0tt + x1tt.t*x1tt

    toc(sigma)

    //print("GDA parameter calculation finished: ")
    //println("  phi = " + phi)
    //println("  mu0 = " ); mu0.pprint
    //println("  mu1 = " ); mu1.pprint
    println("  sigma = "); sigma.pprint
    println(sigma)

  }
}