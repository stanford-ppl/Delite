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

class SparseMatrixSuite extends DeliteSuite {
  def testDataOps() { compileAndTest(SparseMatrixDataOpsRunner) }
}

