package ppl.apps.ml.logreg

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object LogRegRunner extends OptiMLApplicationRunner with LogReg

trait LogReg extends OptiMLApplication {

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LogReg <input matrix file> <output vector file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    val x = readMatrix(args(0))
    val y = readVector(args(1)).t
    
    println("x.numRows: " + x.numRows)
    println("x.numCols: " + x.numCols)
    println("y.length:  " + y.length)

    // gradient descent with logistic function
    tic()
    val w = gradient (SupervisedTrainingSet(x,y), alpha = 1, tol = .001, maxIter = 30) { (t,xi) => 1.0 / (1.0 + exp(t*(-1.0) *:* xi)) }
    toc(w)
    println("w:")
    w.pprint        
  }
}
