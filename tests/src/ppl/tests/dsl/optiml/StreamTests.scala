package ppl.tests.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication, OptiMLExp}
import ppl.dsl.optiml.datastruct.scala.{Vector, Stream}

/* Testing stream operations
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: 3/14/11
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

object StreamTestsRunner extends OptiMLApplicationRunner with StreamTests

trait StreamTests extends OptiMLApplication {

  def testInitSmall() = {
    Stream[Int](1000, 1000){ (i,j) => random[Int] }
  }

  def testInitPureLarge() = {
    Stream[Int](1000000, 10000){ (i,j) => i+j }
  }

  def testInitEffectLarge() = {
    Stream[Int](1000000, 10000){ (i,j) => random[Int] }
  }

  def testForeach(s: Rep[Stream[Int]]) = {
    s.foreachRow { v => println("row sum: " + v.sum) }
  }

  def testStreamCorrectSmall() = {
    val s = Stream[Double](10,10){ (i,j) => i*10+j }
    s.foreachRow { v => v.pprint }
  }

  def testStreamCorrectLarge() = {
    val s = Stream[Double](11000,10){ (i,j) => i*10+j }
    s.foreachRow { v => v.pprint }
  }

  def main() = {
    //tic
    //val s1 = testInitSmall()
    //val s2 = testInitPureLarge()

    //println("Stream isPure (should be true): " + s2.isPure)
    //val s3 = testInitEffectLarge()
    //println("Stream isPure (should be false): " + s3.isPure)
    //toc
    //println("Stream pure foreach   ============================================")
    //testForeach(s2)
    //println("Stream effect foreach ============================================")
    //testForeach(s3)

    println("Stream correctness small ============================================")
    println("with 1 proc, should print row vectors starting at 0, 10, ... 90")
    testStreamCorrectSmall()
    println("Stream correctness large ============================================")
    println("with 1 proc, should print row vectors starting at 0, 10, ... 109990")
    testStreamCorrectLarge()

    //testFileStream
    //toc
  }
}