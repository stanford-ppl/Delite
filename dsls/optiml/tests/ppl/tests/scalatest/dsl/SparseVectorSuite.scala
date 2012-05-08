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
    collect(v(0) == 0)
    collect(v(5) == 10)
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

class SparseVectorSuite extends DeliteSuite {
  def testDataOps() { compileAndTest(SparseVectorDataOpsRunner) }
}

