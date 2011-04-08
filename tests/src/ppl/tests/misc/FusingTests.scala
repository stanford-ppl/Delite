package ppl.tests.misc

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner, OptiMLExp}

object FusingTestsRunner extends OptiMLApplicationRunner with FusingTests

trait FusingTests extends OptiMLApplication {

  def fuseSimple2() {
    val a = Vector.range(1,10)
    val b = Vector.range(10,19)
    val c = Vector.range(20,29)
    val res = a+b+c
    res.pprint
  }

  def fuseMix2() {
    val a = Vector.range(1,10)
    val b = Vector.range(10,19)
    val c = Vector.range(20,29)
    val res = (a+b).t-c.t
    res.pprint
  }

  def fuseSum() {
    val a = Vector.range(1,10)
    println(a.t.sum)
  }

  def main() = {
    fuseSimple2()
    fuseMix2()
    fuseSum()
  }
}