/* Unit tests for OptiML sparse vectors.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 8, 2012
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.tests.scalatest.dsl.optiml

import ppl.tests.scalatest._
import ppl.dsl.optiml._

object SparseVectorDataOpsRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorDataOps
trait SparseVectorDataOps extends DeliteTestModule with OptiMLApplication {
  def main() {
    val v = SparseVector[Double](100,true)
    v(5) = 10
    v(75) = 20

    // update
    collect(v.length == 100)
    collect(v.nnz == 2)
    collect(v(17) == 0)
    collect(v(5) == 10)
    
    // insert, apply
    v.insert(50,1000)
    collect(v.length == 101)
    collect(v(50) == 1000)
    collect(v(5) == 10)
    collect(v(75) == 0)
    collect(v(76) == 20)
    
    // removeAll
    v.removeAll(6,95)
    collect(v.length == 6)
    collect(v == DenseVector[Double](0,0,0,0,0,10))
    
    // insertAll
    val v2 = SparseVector[Double](10,true)
    v2(1) = 2; v2(2) = 2; v2(7) = 2; v2(8) = 2
    v.insertAll(3, v2)
    collect(v.length == 16)
    collect(v == DenseVector[Double](0,0,0,0,2,2,0,0,0,0,2,2,0,0,0,10))
    
    val v3 = SparseVector[Double](10,true)
    v3(0) = 72
    val v4 = v ++ v3
    collect(v4.length == 26)
    
    // trim
    v.trim()
    collect(v.length == 16)
    collect(v == DenseVector[Double](0,0,0,0,2,2,0,0,0,0,2,2,0,0,0,10))
    
    // clear
    v.clear()
    collect(v.length == 0)
    collect(v == DenseVector[Double]())
    
    // copyFrom
    v3 += 1; v3 += 1
    v3.copyFrom(1, v2)
    collect(v3.length == 12)
    collect(v3 == DenseVector[Double](72,0,2,2,0,0,0,0,2,2,0,1))
    
    mkReport
  }
}

object SparseVectorBulkOpsRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorBulkOps
trait SparseVectorBulkOps extends DeliteTestModule with OptiMLApplication {
  def main() {
    val a = Vector.sparse[Int](100000,false)
    a(1023) = 62
    a(23300) = 9
    a(5) = 1
    a(99997) = 17
    
    val b = Vector.sparse[Int](100000,false)
    b(9999) = 328
    b(23300) = 10
    
    val a2 = a mapNZ { e => e + 1 }
    collect(a2.nnz == 4)
    collect(a2.length == 100000)
    collect(a2.isRow == false)
    collect(a2(1023) == 63)   
    collect(a2(23300) == 10)
    collect(a2(5) == 2)
    collect(a2(99997) == 18)
    collect(a2(50000) == 0)
    
    val t1 = a + b
    collect(t1.nnz == 5)
    collect(t1.length == 100000)
    collect(t1.isRow == false)
    collect(t1(1023) == 62)
    collect(t1(23300) == 19)
    collect(t1(5) == 1)
    collect(t1(99997) == 17)
    collect(t1(9999) == 328)
    collect(t1(50000) == 0)    
    
    val t2 = a*b
    collect(t2.nnz == 1)
    collect(t2.length == 100000)
    collect(t2.isRow == false)
    collect(t2(1023) == 0)
    collect(t2(23300) == 90)
    collect(t2(5) == 0)
    collect(t2(99997) == 0)
    collect(t2(9999) == 0)
    collect(t2(50000) == 0)        
    
    val b2 = a.zip(b) { (l,r) => l % (r+2) }
    collect(b2.nnz == 3)
    collect(b2.isRow == false)
    collect(b2.length == 100000)
    collect(b2(1023) == 0)   
    collect(b2(23300) == 9)
    collect(b2(5) == 1)
    collect(b2(99997) == 1)
    collect(b2(9999) == 0)
    collect(b2(50000) == 0)  
    
    mkReport  
  }
}

object SparseVectorAccessorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorAccessors
trait SparseVectorAccessors extends DeliteTestModule with OptiMLApplication {
  def main() {
    val v = Vector.sparse[Double](1000,true) map { e => random[Double] } // not a great sparse matrix

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
    
    val v2 = Vector.sparse[Int](5,true)
    v2(0) = 1; v2(1) = 2; v2(2) = 3; v2(3) = 4; v2(4) = 5
    val vSlice = v2(0::2)
    collect(vSlice == DenseVector(1,2))
    val vSlice2 = v2(3::5)
    collect(vSlice2 == DenseVector(4,5))
    val vSlice3 = v2(4,2,0)
    collect(vSlice3 == DenseVector(5,3,1))    

    mkReport
  }
}

object SparseVectorOperatorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorOperators
trait SparseVectorOperators extends DeliteTestModule with OptiMLApplication {
  def main() {
    val v = Vector.sparse[Double](1000,true)

    val vt = v.t
    collect(vt.isRow != v.isRow)

    //val vc = v.clone
    //collect(vc.cmp(v) == true)

    val v2 = Vector.sparse[Double](5,true)
    v2(0) = 1; v2(1) = 2; v2(2) = 3; v2(3) = 4; v2(4) = 5    
    //collect(median(v2) == 3)
    collect(mean(v2) == 3)
    collect(max(v2) == 5)
    collect(min(v2) == 1)
    collect(mean(3,6,2,5) == 4.0)
    
    mkReport
  }
}

object SparseVectorCountRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorCount
trait SparseVectorCount extends DeliteTestModule with OptiMLApplication {
  def main() = {
    
    val v2 = Vector.sparse[Double](100,true)
    v2(0) = 1; v2(1) = 2; v2(2) = 2; v2(3) = 2; v2(4) = 5    
    val c = v2.count { _ == 2 }
    collect(c == 3)
    mkReport
  }
}

object SparseVectorBulkUpdateRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorBulkUpdate
trait SparseVectorBulkUpdate extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = Vector.sparse[Double](10,true)
    val i = (0::5)
    v(i) = 1
    collect(v == DenseVector[Double](1,1,1,1,1,0,0,0,0,0))
    mkReport
  }
}

object SparseVectorFindRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorFind
trait SparseVectorFind extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v2 = Vector.sparse[Double](100,true)
    v2(0) = 1; v2(1) = 2; v2(2) = 2; v2(3) = 2; v2(4) = 5        
    val i: Rep[DenseVector[Int]] = v2.find { _ == 2 }
    collect(i == DenseVector(1,2,3)) 
    mkReport
  }
}

object SparseVectorDistRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorDist
trait SparseVectorDist extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v1 = Vector.sparse[Double](100,true)
    v1(0) = 10; v1(1) = 10; v1(2) = 5; v1(3) = 5; v1(4) = 0        
    
    val v2 = Vector.sparse[Double](100,true)
    v2(0) = 5; v2(1) = 5; v2(2) = 10; v2(3) = 10; v2(4) = -5        
    
    collect(dist(v1,v2) == 25)
    mkReport
  }
}

object SparseVectorDistinctRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorDistinct
trait SparseVectorDistinct extends DeliteTestModule with OptiMLApplication {
  def main() = {
    
    val v1 = Vector.sparse[Double](100,true)
    v1(0) = 10; v1(1) = 10; v1(2) = 5; v1(3) = 5; v1(4) = 0        
    
    collect(v1.contains(5.))
    collect(!v1.contains(7.5))
    collect(v1.distinct == DenseVector(10.,5.,0.))
    mkReport
  }
}

object SparseVectorMedianRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseVectorMedian
trait SparseVectorMedian extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val v = Vector.sparse[Int](9,true)
    v(0) = 1; v(1) = 5; v(2) = 3; v(3) = 4; v(4) = 2; v(5) = 6; v(6) = 7; v(7) = 8; v(8) = 9        
    
    collect(v.median == 5)
    mkReport
  }
}

class SparseVectorSuite extends DeliteSuite {
  def testDataOps() { compileAndTest(SparseVectorDataOpsRunner) }
  def testBulkOps() { compileAndTest(SparseVectorBulkOpsRunner) }
  
  def testAccessors() { compileAndTest(SparseVectorAccessorsRunner) }
  def testOperators() { compileAndTest(SparseVectorOperatorsRunner) }
  def testCount() { compileAndTest(SparseVectorCountRunner) }
  def testBulkUpdate() { compileAndTest(SparseVectorBulkUpdateRunner) }
  def testFind() { compileAndTest(SparseVectorFindRunner) }
  def testDist() { compileAndTest(SparseVectorDistRunner) }
  def testDistinct() { compileAndTest(SparseVectorDistinctRunner) }
  
  // sparse sort not supported yet
  //def testMedian() { compileAndTest(SparseVectorMedianRunner) }
}

