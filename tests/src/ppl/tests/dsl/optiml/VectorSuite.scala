/* Unit tests for OptiML vectors.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 22, 2010
 * modified: Mar 31, 2011
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.tests.dsl.optiml

import ppl.dsl.optiml.datastruct.scala.{Vector,RangeVector}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

object VectorAccessorsRunner extends OptiMLApplicationRunner with VectorAccessors
trait VectorAccessors extends OptiMLTestModule {
  def main() {
    implicit val collector = Vector[Boolean]()    
    val v = Vector.rand(1000)

    collect(v.length == 1000)

    val elem = v(92)
    collect(v(92) == elem)

    val first = v.first
    collect(first == v(0))

    val last = v.last
    collect(last == v(v.length-1))

    val twenty = v.slice(30, 50)
    collect(twenty.length == 20)
    var i = 0
    while (i < twenty.length){
      collect(twenty(i) == v(i+30))
      i += 1
    }

    val firstTen = v.take(10)
    collect(firstTen.length == 10)
    i = 0
    while (i < firstTen.length){
      collect(firstTen(i) == v(i))
      i += 1
    }

    val allExceptTen = v.drop(10)
    collect(allExceptTen.length == (v.length - 10))
    i = 0
    while (i < allExceptTen.length){
      collect(allExceptTen(i) == v(i+10))
      i += 1
    }

    mkReport
  }
}

object VectorOperatorsRunner extends OptiMLApplicationRunner with VectorOperators
trait VectorOperators extends OptiMLTestModule {
  def main() {
    implicit val collector = Vector[Boolean]()
    val v = Vector.rand(1000)

    val vt = v.t
    collect(vt.isRow != v.isRow)

    //val vc = v.clone
    //collect(vc.cmp(v) == true)

    mkReport
  }
}

object VectorUpdatesRunner extends OptiMLApplicationRunner with VectorUpdates
trait VectorUpdates extends OptiMLTestModule {
  def main() {
    implicit val collector = Vector[Boolean]()
    val v = Vector.rand(1000).mutable
    val vb = Vector.rand(10).mutable

    v(7) = 0.9123
    collect(v(7) == 0.9123)

    val twov = (v ++ v)
    collect(twov.length == v.length*2)
    collect(twov(1000) == v(0))

    var vlen = v.length
    v += 9.2
    collect(v.length == vlen + 1)
    collect(v(vlen) == 9.2)
    vlen += 1

    v ++= vb
    collect(v.length == vlen+vb.length)
    vlen += vb.length
    var i = 0
    while (i < vb.length){
      collect(v(vlen-vb.length+i) == vb(i))
      i += 1
    }

    v.copyFrom(100, vb)
    i = 0
    while (i < vb.length){
      collect(v(i+100) == vb(i))
      i += 1
    }

    v.insert(500, 9.21)
    collect(v.length == vlen+1)
    collect(v(500) == 9.21)
    vlen += 1

    v.insertAll(13, vb)
    collect(v.length == vlen + vb.length)
    i = 0
    while (i < vb.length){
      collect(v(i+13) == vb(i))
      i += 1
    }
    vlen += vb.length

    var shifted = v(72)
    v.remove(71)
    collect(v.length == vlen-1)
    collect(v(71) == shifted)
    vlen -= 1

    shifted = v(102)
    v.removeAll(99,3)
    collect(v.length == vlen-3)
    collect(v(99) == shifted)
    vlen -= 3

    v.trim
    collect(v.length == vlen)

    mkReport
  }
}

object VectorRangeRunner extends OptiMLApplicationRunner with VectorRange
trait VectorRange extends OptiMLTestModule {
  def main() {
    implicit val collector = Vector[Boolean]()
    val rangeEasy = Vector.range(0, 1000)
    val rangeHard = Vector.range(11, 100, 2)

    collect(rangeEasy(0) == 0)
    collect(rangeEasy(500) == 500)
    collect(rangeEasy(999) == 999)
    collect(rangeEasy.length == 1000)

    collect(rangeHard(0) == 11)
    collect(rangeHard(1) == 13)
    collect(rangeHard(2) == 15)
    collect(rangeHard(44) == 99)
    collect(rangeHard.length == 45)

    mkReport
  }
}

class VectorSuite extends OptiMLSuite {
  def testAccessors() { compileAndTest(VectorAccessorsRunner) }
  def testOperators() { compileAndTest(VectorOperatorsRunner) }
  def testUpdates() { compileAndTest(VectorUpdatesRunner) }
  def testRange() { compileAndTest(VectorRangeRunner) }
}

