/* Unit tests for OptiML sparse matrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 23, 2012
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.tests.scalatest.dsl.optiml

import ppl.tests.scalatest._
import ppl.dsl.optiml._

object SparseMatrixDataOpsRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseMatrixDataOps
trait SparseMatrixDataOps extends DeliteTestModule with OptiMLApplication {
  def main() {
    val mb = Matrix.sparse[Double](1000,1000)
    mb(10,100) = 5
    mb(9,100) = 1
    mb(9,722) = 722
    mb(9,331) = 331
    mb(11,101) = 2
    mb(573,71) = 15
    mb(200,17) = 3

    val m = mb.finish
    collect(m.numRows == 1000)
    collect(m.numCols == 1000)
    collect(m.nnz == 7)
    collect(m(10,10) == 0)
    collect(m(0,100) == 0)
    collect(m(9,100) == 1)
    collect(m(9,722) == 722)
    collect(m(9,331) == 331)
    collect(m(9,500) == 0)
    collect(m(10,772) == 0)
    collect(m(10,100) == 5)
    collect(m(11,101) == 2)
    collect(m(200,17) == 3)
    collect(m(573,71) == 15)
    collect(m(500,500) == 0)
    collect(m(10,101) == 0)
    collect(m(200,71) == 0)
    
    // insert/remove rows/col
    val mb2 = Matrix.sparse[Int](10,10)
    mb2.insertCol(5,DenseVector(1,2,3,4,5,6,7,8,9,10))
    mb2.insertCol(10,DenseVector(0,0,0,0,25,0,0,0,0,0))
    mb2.removeCols(2,2)
    val m2 = mb2.finish
    collect(m2.numRows == 10)
    collect(m2.numCols == 10)
    collect(m2.nnz == 11)
    collect(m2(0,3) == 1)
    collect(m2(4,8) == 25)

    val mb3 = Matrix.sparse[Int](10,10)
    mb3.insertRow(5,DenseVector(1,2,3,4,5,6,7,8,9,10))
    mb3.insertRow(10,DenseVector(0,0,0,0,25,0,0,0,0,0))
    mb3.removeRows(3,1)
    val m3 = mb3.finish
    collect(m3.numRows == 11)
    collect(m3.numCols == 10)
    collect(m3.nnz == 11)
    collect(m3(4,0) == 1)
    collect(m3(9,4) == 25)
    
    mkReport
  }
}

object SparseMatrixBulkOpsRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseMatrixBulkOps
trait SparseMatrixBulkOps extends DeliteTestModule with OptiMLApplication {
  def main() {
    val ab = Matrix.sparse[Int](10000,10000)
    ab(1023,39) = 62
    ab(777,2330) = 9
    ab(5,7534) = 1
    ab(1373,9999) = 17
    val a = ab.finish
    
    val bb = Matrix.sparse[Int](10000,10000)
    bb(9999,7534) = 328
    bb(777,2330) = 10
    val b = bb.finish
    
    // val a2 = a mapNZ { e => e + 1 }
    // collect(a2.nnz == 4)
    // collect(a2.numRows == 100000)
    // collect(a2.numCols == 100000)
    // collect(a2(1023,39) == 63)   
    // collect(a2(777,23300) == 10)
    // collect(a2(5,75349) == 2)
    // collect(a2(13733,99997) == 18)
    // collect(a2(50000,50000) == 0)
    
    val t1 = a + b
    collect(t1.nnz == 5)
    collect(t1.numRows == 10000)
    collect(t1.numCols == 10000)    
    collect(t1(1023,39) == 62)
    collect(t1(777,2330) == 19)
    collect(t1(5,7534) == 1)
    collect(t1(1373,9999) == 17)
    collect(t1(9999,7534) == 328)
    collect(t1(5000,5000) == 0)    
    
    val t2 = a*:*b
    collect(t2.nnz == 1)
    collect(t2.numRows == 10000)
    collect(t2.numCols == 10000)    
    collect(t2(1023,39) == 0)
    collect(t2(777,2330) == 90)
    collect(t2(5,7534) == 0)
    collect(t2(1373,9999) == 0)
    collect(t2(9999,7534) == 0)
    collect(t2(5000,5000) == 0)        
    
    val b2 = a.zip(b) { (l,r) => l % (r+2) }
    collect(b2.nnz == 3) 
    collect(b2.numRows == 10000)
    collect(b2.numCols == 10000)    
    collect(b2(1023,39) == 0) 
    collect(b2(777,2330) == 9)
    collect(b2(5,7534) == 1)
    collect(b2(1373,9999) == 1) 
    collect(b2(9999,7534) == 0)
    collect(b2(5000,5000) == 0)  
    
    mkReport  
  }
}

class SparseMatrixSuite extends DeliteSuite {
  def testDataOps() { compileAndTest(SparseMatrixDataOpsRunner) }
  def testBulkOps() { compileAndTest(SparseMatrixBulkOpsRunner) }
}

