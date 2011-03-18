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


  def testCount() = {
    val v = Vector(1,2,3,5,5,5,7,8,9,10)
    val c = v.count { _ == 5 }
    println("==== test count")
    println("should be 3: " + c)
  }

  def testBulkUpdate() = {
    val v = Vector.mzeros(10)
    val i = (0::5)
    v(i) = 1
    println("==== test bulk update")
    println("should be [1 1 1 1 1 0 0 0 0 0]")
    v.pprint
  }

  def testFind() = {
    val v = Vector(1,2,3,5,5,5,7,8,9,10)
    val i = v.find { _ == 5 }
    println("==== test find")
    println("should be [3 4 5]")
    i.pprint
  }

  def testDist() = {
    val v1 = Vector(10.,10.,5.,5.,0.)
    val v2 = Vector(5.,5.,10.,10.,-5.)
    println("==== test dist")
    println("should be 25: " + dist(v1, v2))
  }

  def testMedian() = {
    val v = Vector(1,5,3,4,2,6,7,8,9)
    println("==== test median")
    println("should be 5: " + v.median)
  }

  def testNearestNeighbor() = {
    val m = Matrix(Vector(1,1,1,1), Vector(9,9,9,9), Vector(-2,-2,-2,-2), Vector(0,0,0,0), Vector(1,1,1,1))
    val nearestUnique = nearestNeighborIndex(0, m, false)
    println("==== test nearestNeighbor")
    println("should be 3: " + nearestUnique)
    val nearestAny = nearestNeighborIndex(0, m)
    println("should be 4: " + nearestAny)
  }

  def testSample() = {
    val v = (0::100)
    val vs = sample(v, 10)
    println("==== test sample")
    println("original vector: ")
    v.pprint
    println("sampled vector: ")
    vs.pprint
  }

  def main() = {
    // test args
//    for (a <- args)
//      println(a)

    //testInit()
    //testLoop()

    testCount()
    testBulkUpdate()
    testFind()
    testDist()
    testMedian()
    testNearestNeighbor()
    testSample()
  }
}