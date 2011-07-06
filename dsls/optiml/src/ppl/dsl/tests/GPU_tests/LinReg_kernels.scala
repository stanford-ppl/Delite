package ppl.dsl.tests.GPU_tests

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication


object LinReg_kernelsRunner extends OptiMLApplicationRunner with LinReg_kernels

trait LinReg_kernels extends OptiMLApplication {

  def main() {
    println("LinReg kernel generation")
    
    val X = Matrix[Double](10,10)
    val Xt = X.t
    val xref = Matrix[Double](10,10)
    val x = Vector[Double](10)
    val y = Vector[Double](10)
    val e = 5
    val tau = 10
    
    val x_cur = xref(e,1)
    val weights = x.map(ele => Math.exp(-.1*(x_cur-ele)*(x_cur-ele)/(2.0*tau*tau))/2.0)
    println("weights = "); weights.pprint
    val W = Matrix.diag(weights.length, weights)
    println("W = "); W.pprint
    // GPU kernels for below two operations will be generated with CUBLAS
    //val t1 = Xt*W
    //val theta = ((t1*X).inv)*(t1*y) // relaxed v_prod, ignore is_row on y
  }
}