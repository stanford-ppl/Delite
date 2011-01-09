package ppl.tests.dsl

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLExp}

/* Tests the generated code functionality for Delite ops, using OptiML data structures.
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: 12/13/10
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

object DeliteOpTests extends DeliteApplication with OptiMLExp {

  // kernels
  def testMapK() = {
    val v = Vector[Double](1000,true)
    val v2 = v map { e => 10 }
    v2.pprint
  }

  def testZipK() = {
    val v1 = Vector[Double](1000,true)
    val v2 = Vector[Double](1000,true) map { e => 2. }
    val v3 = v1 + v2
    v3.pprint
  }

  def testReduceK() = {
    val v = Vector.range(0, 1000)
    println(v.sum)
  }

  def testMapReduceK() = {
    val v = Vector.range(0, 1000)
    val x = sum(0, v.length) { i => v(i) }
    println(x)
  }

  def testForeachK() = {
    val v = Vector.range(0, 10)
    for (e <- v) {
      if ((e > 0) && (e < v.length-1)) {
        println(v(e-1) + v(e+1))
      }
    }
  }


  // straight-line (nested)
  def testMapS() = {
    val res = Vector[Double](1,true) map { e =>
      val v = Vector[Double](1000,true)
      v map { e => 10 }
    }
    res(0).pprint
  }

  def testZipS() = {
    val res = Vector[Double](1,true) map { e =>
      val v1 = Vector[Double](1000,true)
      val v2 = Vector[Double](1000,true) map { e => 2. }
      v1 + v2
    }
    res(0).pprint
  }

  def testReduceS() = {
    val res = Vector[Double](1,true) map { e =>
      val v = Vector.range(0, 1000)
      v.sum
    }
    println(res(0))
  }

  def testMapReduceS() = {
    val res = Vector[Double](1,true) map { e =>
      val v = Vector.range(0, 1000)
      sum(0, v.length) { i => v(i) }
    }
    println(res(0))
  }

  def testForeachS() = {
    val res = Vector[Double](1,true) map { e =>
      val v = Vector.range(0, 10)
      for (e <- v) {
        if ((e > 0) && (e < v.length-1)) {
          println(v(e-1) + v(e+1))
        }
      }
    }
    println(res(0))
  }


  def main() = {
    testMapK()
    testZipK()
    testReduceK()
    testMapReduceK()
    testForeachK()

    testMapS()
    testZipS()
    testReduceS()
    testMapReduceS()
    testForeachS()
  }

}