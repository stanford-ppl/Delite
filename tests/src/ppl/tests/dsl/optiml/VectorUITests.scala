package ppl.tests.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication, OptiMLExp}

/* Testing vector operations
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: 12/24/10
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

object VectorUITestsRunner extends OptiMLApplicationRunner with VectorUITests

trait VectorUITests extends OptiMLApplication {

  def testInit() = {
    val v1 = Vector(1,2,3,4,5)
    val v2 = Vector(1.,2.,3.,4.,5.)
    v1.pprint
    v2.pprint
  }

  def testLoop() = {
   val vec1 = Vector.rand(5)
   val vec2 = Vector.rand(5)

   var idx = unit(0)
   while(idx < 0) {
     vec2(idx) = vec1(idx)
   }
   vec2.pprint
  }

  def testConversions() = {
    // TODO: test int*double, double*int, vec[int]*vec[double], vec[double]*vec[int]
  }

  def main() = {
    // test args
    for (a <- args)
      println(a)

    //testInit()
    //testLoop()

  }
}