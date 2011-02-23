package ppl.dsl.tests.blasTest

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object MatMultVTestRunner extends OptiMLApplicationRunner with MatMultVTestRunner

trait MatMultVTestRunner extends OptiMLApplication {

  def main() = {
    val mat_numRows = 12
    val mat_numCols = 20
    val vec_length = 20
		
    val mat = Matrix[Double](mat_numRows,mat_numCols)
    val vec = Vector[Double](vec_length,true)
    val out_correct = Vector[Double](mat_numRows,false)

    /* Initialize */
    for(i <- 0 until mat_numRows) {
      for(j <- 0 until mat_numCols) {
        mat(i,j) = i*j
      }
    }
    for(i <- 0 until vec_length) {
      vec(i) = vec_length - i
    }

    /* Calculate using BLAS */
    val out = mat * vec 

    /* Naive implementation */
    for(i <- 0 until mat_numRows) {
      out_correct(i) = mat(i,0) * vec(0)
      for(j <- 0 until mat_numCols) {
        out_correct(i) = out_correct(i) + mat(i,j) * vec(j)
      }
    }

    /* Compare */
    for(i <- 0 until mat_numRows) {
        if(out(i)!=out_correct(i)) println("Not Same!")
    }

    println("out = "); out.pprint
    println("out_correct = "); out_correct.pprint

    println("MatMultV Test done")
  }
}
