package ppl.dsl.tests.blasTest

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object MatMultTest extends DeliteApplication with OptiMLExp {

  def main() = {
    val in1_numRows = 12
    val in1_numCols = 20
    val in2_numRows = 20
    val in2_numCols = 17
		
    val in1 = Matrix[Double](in1_numRows,in1_numCols)
    val in2 = Matrix[Double](in2_numRows,in2_numCols)
    val out_correct = Matrix[Double](in1_numRows,in2_numCols)

    /* Initialize */
    for(i <- 0 until in1_numRows) {
      for(j <- 0 until in1_numCols) {
        in1(i,j) = i*j
      }
    }
    for(i <- 0 until in2_numRows) {
      for(j <- 0 until in2_numCols) {
        in2(i,j) = i+j
      }
    }

    /* Calculate using BLAS */
    val out = in1 * in2

    /* Naive implementation */
    for(i <- 0 until in1_numRows) {
      for(j <- 0 until in2_numCols) {
        var temp = unit(0.0)
        for(k <- 0 until in1_numCols) {
          temp = temp + in1(i,k)*in2(k,j)
        }
        out_correct(i,j) = temp
      }
    }

    /* Compare */
    for(i <- 0 until in1_numRows) {
      for(j <- 0 until in2_numCols) {
        if(out(i,j)!=out_correct(i,j)) println("Not Same!")
      }
    }

    println("out = "); out.pprint
    println("out_correct = "); out_correct.pprint

    println("MatMult Test done")
  }
}
