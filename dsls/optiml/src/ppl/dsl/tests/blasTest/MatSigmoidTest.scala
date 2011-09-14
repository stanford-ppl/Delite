package ppl.dsl.tests.blasTest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object MatSigmoidTestRunner extends OptiMLApplicationRunner with MatSigmoidTest

trait MatSigmoidTest extends OptiMLApplication {

  def main() = {
    val mat_numRows = 12
    val mat_numCols = 20
		
    val mat = Matrix[Double](mat_numRows,mat_numCols)
    val out_correct = Matrix[Double](mat_numRows,mat_numCols)

    /* Initialize */
    for(i <- 0 until mat_numRows) {
      for(j <- 0 until mat_numCols) {
        mat(i,j) = i*j
      }
    }

    /* Calculate using BLAS */
    val out = mat.sigmoid

    /* Naive implementation */
    for(i <- 0 until mat_numRows) {
      for(j <- 0 until mat_numCols) {
        out_correct(i,j) = 1.0 / ( 1.0 + Math.exp(mat(i,j)*(-1.0)))
      }
    }

    /* Compare */
    for(i <- 0 until mat_numRows) {
      for(j <- 0 until mat_numCols) {
        if(out(i,j)!=out_correct(i,j)) println("Not Same!")
      }
    }

    println("out = "); out.pprint
    println("out_correct = "); out_correct.pprint

    println("MatSigmoid Test done")
  }
}
