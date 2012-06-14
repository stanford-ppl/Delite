/* Unit tests for OptiML vectors.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 22, 2010
 * modified: Mar 31, 2011
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.tests.scalatest.dsl.optiml

import ppl.dsl.optiml.{Vector,DenseVector,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.tests.scalatest._

object VectorAccessorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorAccessors
trait VectorAccessors extends DeliteTestModule with OptiMLApplication {
  def main() {
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
    
    val v2 = Vector(1,2,3,4,5)
    val vSlice = v2(0::2)
    collect(vSlice == Vector(1,2))
    val vSlice2 = v2(3::5)
    collect(vSlice2 == Vector(4,5))
    val vSlice3 = v2(4,2,0)
    collect(vSlice3 == Vector(5,3,1))    

    mkReport
  }
}

object VectorOperatorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorOperators
trait VectorOperators extends DeliteTestModule with OptiMLApplication {
  def main() {
    val v = Vector.rand(1000)

    val vt = v.t
    collect(vt.isRow != v.isRow)

    //val vc = v.clone
    //collect(vc.cmp(v) == true)

    val v2 = Vector(1,2,3,4,5)
    collect(median(v2) == 3)
    collect(mean(v2) == 3)
    collect(max(v2) == 5)
    collect(min(v2) == 1)
    collect(mean(3,6,2,5) == 4.0)
    
    mkReport
  }
}

object VectorUpdatesRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorUpdates
trait VectorUpdates extends DeliteTestModule with OptiMLApplication {
  def main() {
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

object VectorRangeRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorRange
trait VectorRange extends DeliteTestModule with OptiMLApplication {
  def main() {
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

object InitRunner extends DeliteTestRunner with OptiMLApplicationRunner with Init
trait Init extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v1 = Vector(1,2,3,4,5)
    val v2 = Vector(1.,2.,3.,4.,5.)

    // just make sure we can reach here without an error
    collect(true)
    mkReport
  }
}

object LoopRunner extends DeliteTestRunner with OptiMLApplicationRunner with Loop
trait Loop extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val vec1 = Vector.rand(5)
    val vec2 = Vector.rand(5).mutable

    var idx = 0
    while(idx < 5) {
      vec2(idx) = vec1(idx)
      idx += 1
    }
    collect(vec2 == vec1)

    mkReport
  }
}

object CountRunner extends DeliteTestRunner with OptiMLApplicationRunner with Count
trait Count extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = Vector(1,2,3,5,5,5,7,8,9,10)
    val c = v.count { _ == 5 }
    collect(c == 3)
    mkReport
  }
}

object BulkUpdateRunner extends DeliteTestRunner with OptiMLApplicationRunner with BulkUpdate
trait BulkUpdate extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = Vector.zeros(10).mutable
    val i = (0::5)
    v(i) = 1
    collect(v == Vector[Double](1,1,1,1,1,0,0,0,0,0))
    mkReport
  }
}

object FindRunner extends DeliteTestRunner with OptiMLApplicationRunner with Find
trait Find extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = Vector(1,2,3,5,5,5,7,8,9,10)
    val i: Rep[DenseVector[Int]] = v.find { _ == 5 }  // AKS FIXME: the override for __equal doesn't work with IndexVectorDense because of the higher kinded type
    collect(i == Vector(3,4,5))
    mkReport
  }
}

object DistRunner extends DeliteTestRunner with OptiMLApplicationRunner with Dist
trait Dist extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v1 = Vector(10.,10.,5.,5.,0.)
    val v2 = Vector(5.,5.,10.,10.,-5.)

    collect(dist(v1,v2) == 25)
    mkReport
  }
}

object DistinctRunner extends DeliteTestRunner with OptiMLApplicationRunner with Distinct
trait Distinct extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v1 = Vector(10.,10.,5.,5.,0.)

    collect(v1.contains(5.))
    collect(!v1.contains(7.5))
    collect(v1.distinct == Vector(10.,5.,0.))
    mkReport
  }
}

object MedianRunner extends DeliteTestRunner with OptiMLApplicationRunner with Median
trait Median extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = Vector(1,5,3,4,2,6,7,8,9)
    collect(v.median == 5)
    mkReport
  }
}

object NearestNeighborRunner extends DeliteTestRunner with OptiMLApplicationRunner with NearestNeighbor
trait NearestNeighbor extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val m = Matrix(Vector(1,1,1,1), Vector(9,9,9,9), Vector(-2,-2,-2,-2), Vector(0,0,0,0), Vector(1,1,1,1))
    val nearestUnique = nearestNeighborIndex(0, m, false)
    collect(nearestUnique == 3)
    val nearestAny = nearestNeighborIndex(0, m)
    collect(nearestAny == 4)
    mkReport
  }
}

object SampleRunner extends DeliteTestRunner with OptiMLApplicationRunner with Sample
trait Sample extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = (0::100)
    val vs = sample[Int,DenseVector[Int]](v, 10)
    //val vs = sample(v, 10)
    vs foreach { e => collect(v contains e) }
    mkReport
  }
}

object GroupByRunner extends DeliteTestRunner with OptiMLApplicationRunner with GroupBy
trait GroupBy extends DeliteTestModule with OptiMLApplication {
  def main() = {
    
    val v = Vector("one", "two", "two", "three", "four", "three", "four", "three", "four", "four")
    val vs = v.groupBy(e=>e)
    collect(vs.length == 4)
    for (v <- vs) { 
      if (v(0) == "one") collect(v.length == 1)
      else if (v(0) == "two") collect(v.length == 2)
      else if (v(0) == "three") collect(v.length == 3)
      else if (v(0) == "four") collect(v.length == 4)
    }
    mkReport
  }
}

class VectorSuite extends DeliteSuite {
  def testAccessors() { compileAndTest(VectorAccessorsRunner) }
  def testOperators() { compileAndTest(VectorOperatorsRunner) }
  def testUpdates() { compileAndTest(VectorUpdatesRunner) }
  def testRange() { compileAndTest(VectorRangeRunner) }
  
  def testInit() { compileAndTest(InitRunner) }
  def testLoop() { compileAndTest(LoopRunner) }
  def testCount() { compileAndTest(CountRunner) }
  def testBulkUpdate() { compileAndTest(BulkUpdateRunner) }
  def testFind() { compileAndTest(FindRunner) }
  def testDist() { compileAndTest(DistRunner) }
  def testDistinct() { compileAndTest(DistinctRunner) }
  def testMedian() { compileAndTest(MedianRunner) }
  def testNearestNeighbor() { compileAndTest(NearestNeighborRunner) }
  def testSample() { compileAndTest(SampleRunner) }
  def testGroupBy() { compileAndTest(GroupByRunner) }
}

