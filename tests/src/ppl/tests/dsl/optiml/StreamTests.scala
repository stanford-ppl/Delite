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

  def testInitLarge() = {
    Stream[Int](1000000, 10000){ (i,j) => random[Int] }
  }

  def testForeach(s: Rep[Stream[Int]]) = {
    s.foreachRow { v => println("row sum: " + v.sum) }
  }

  def main() = {
    tic
    val s1 = testInitSmall()
    val s2 = testInitLarge()
    toc
    testForeach(s2)
  }
}