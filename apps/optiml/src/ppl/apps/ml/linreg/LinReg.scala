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


  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LinRegSerial <input matrix file> <output vector file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    val x = readMatrix(args(0))
    val y = readVector(args(1)).t

//    logElapsed("Input Section Complete")

    val theta = linreg.unweighted(x,y)
    println("Unweighted linear regression")
    println("theta: ")
    theta.pprint
    print("\\n")

    tic()
    val guess = linreg.weighted(x,y)
    toc(guess)

    println("Locally weighted linear regression")
    println("guess: ")
    guess.pprint
    print("\\n")

    //PerformanceTimer.save("LinReg")
  }
}
