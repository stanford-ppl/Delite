package ppl.tests.scalatest.dsl.optiml

import ppl.dsl.optiml._
import scala.virtualization.lms.util.OverloadHack
import ppl.tests.scalatest._

/**
 * These simply test that Vector/Matrix arithmetic semantics don't change when using sparse.
 * This test is not supposed to represent idiomatic usage of sparse vectors or matrices.
 * All of the tests in here are copies of the dense test; the actual vectors and matrices
 * are dense, but stored (inefficiently) as sparse.
 */
object SparseSimpleVectorArithmeticRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseSimpleVectorArithmetic
trait SparseSimpleVectorArithmetic extends DeliteTestModule with LinearAlgebraTestsCommon {
  def main() = {
    val rowA = SparseVector[Double](3, true)
    rowA(0) = 11
    rowA(1) = 22
    rowA(2) = 33
    
    val rowB = SparseVector[Double](3, true)
    rowB(0) = -5.3
    rowB(1) = -17.2
    rowB(2) = -131
    
    val rowD = SparseVector[Double](2,true)
    rowD(0) = -1.1
    rowD(1) = -6.2
    
    val colC = SparseVector[Double](3,false)
    colC(0) = 7
    colC(1) = 3.2
    colC(2) = 13.3

    // A*B piecewise
    val ansVec = rowA*rowB
    collect(check(ansVec, Vector(-58.3, -378.4, -4323.)))

    // dot product
    val ansDbl = rowA *:* colC
    collect(check(ansDbl, 586.3))

    // outer product
    val ansMat = colC ** rowA
    collect(check(ansMat, Matrix(Vector(77., 154., 231.), Vector(35.2, 70.4, 105.6), Vector(146.3, 292.6, 438.9))))

    mkReport
  }
}

object SparseSimpleMatrixArithmeticRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseSimpleMatrixArithmetic
trait SparseSimpleMatrixArithmetic extends DeliteTestModule with LinearAlgebraTestsCommon {
  def main() = {
    val rowA = SparseVector[Double](3, true)
    rowA(0) = 11
    rowA(1) = 22
    rowA(2) = 33
    
    val rowB = SparseVector[Double](3, true)
    rowB(0) = -5.3
    rowB(1) = -17.2
    rowB(2) = -131
    
    val colC = SparseVector[Double](3,false)
    colC(0) = 7
    colC(1) = 3.2
    colC(2) = 13.3
    
    val m33b = Matrix.sparse[Double](0,0)
    m33b += rowA
    m33b += rowB
    m33b += colC.t
    val m33 = m33b.finish
    
    val v1 = SparseVector[Double](3,true)
    v1(0) = 3.5
    v1(1) = 7.5
    v1(2) = 9.0
    
    val v2 = SparseVector[Double](3,true)
    v2(0) = -5.6
    v2(1) = 8.2
    v2(2) = 17.3
    val m23b = Matrix.sparse[Double](0,0)
    m23b += v1
    m23b += v2
    val m23 = m23b.finish
    
    val v3 = SparseVector[Double](2,true)
    v3(0) = .07
    v3(1) = .91
    
    val v4 = SparseVector[Double](2,true)
    v4(0) = 17
    v4(1) = -10
    
    val v5 = SparseVector[Double](2,true)
    v5(0) = -99
    v5(1) = .023
    
    val m32b = Matrix.sparse[Double](0,0)
    m32b += v3
    m32b += v4
    m32b += v5
    val m32 = m32b.finish

    // matrix square multiplication
    // val ansMat = m33*m33
    // collect(check(ansMat, Matrix(Vector(235.4, -30.8, -2080.1), Vector(-884.14, -239.96, 336.),
    //                              Vector(153.14, 141.52, -11.31))))

    // inverse
    // val ansMat2 = m33.inv
    // collect(check(ansMat2, Matrix(Vector(-.0145, 0.0143, 0.1765), Vector(0.0645, 0.0065, -0.0965),
    //                              Vector(-0.0079, -0.0091, 0.0055))))

    // matrix transpose
    val ansMat3 = m33.t
    collect(check(ansMat3, Matrix(Vector(11., -5.3, 7.), Vector(22., -17.2, 3.2), Vector(33., -131., 13.3))))

    // matrix multiplication
    // val ansMat4 = m33*m32
    // collect(check(ansMat4, Matrix(Vector(-2892.223, -209.2310), Vector(12676.229, 164.1640), Vector(-1261.81, -25.3241))))

    // chained matrix multiplication
    // val ansMat5 = m23*m33*m32
    // collect(check(ansMat5, Matrix( Vector(73592.6225, 271.0046), Vector(98312.252799, 2079.73147))))

    // summation
    val ans = m23.sum
    collect(check(ans, 39.9))

    mkReport
  }
}

object SparseCombinedVecMatArithmeticRunner extends DeliteTestRunner with OptiMLApplicationRunner with SparseCombinedVecMatArithmetic
trait SparseCombinedVecMatArithmetic extends DeliteTestModule with LinearAlgebraTestsCommon {
  def main() = {
    val rowA = SparseVector[Double](3, true)
    rowA(0) = 11
    rowA(1) = 22
    rowA(2) = 33
    
    val rowB = SparseVector[Double](3, true)
    rowB(0) = -5.3
    rowB(1) = -17.2
    rowB(2) = -131
    
    val rowD = SparseVector[Double](2,true)
    rowD(0) = -1.1
    rowD(1) = -6.2
    
    val colC = SparseVector[Double](3,false)
    colC(0) = 7
    colC(1) = 3.2
    colC(2) = 13.3
    
    val colEt = SparseVector[Double](2,true)
    colEt(0) = .05
    colEt(1) = 9.97
    val colE = colEt.t

    val m33b = Matrix.sparse[Double](0,0)
    m33b += rowA
    m33b += rowB
    m33b += colC.t
    val m33 = m33b.finish
    
    val v1 = SparseVector[Double](3,true)
    v1(0) = 3.5
    v1(1) = 7.5
    v1(2) = 9.0
    
    val v2 = SparseVector[Double](3,true)
    v2(0) = -5.6
    v2(1) = 8.2
    v2(2) = 17.3
    val m23b = Matrix.sparse[Double](0,0)
    m23b += v1
    m23b += v2
    val m23 = m23b.finish
    
    val v3 = SparseVector[Double](2,true)
    v3(0) = .07
    v3(1) = .91
    
    val v4 = SparseVector[Double](2,true)
    v4(0) = 17
    v4(1) = -10
    
    val v5 = SparseVector[Double](2,true)
    v5(0) = -99
    v5(1) = .023
    
    val m32b = Matrix.sparse[Double](0,0)
    m32b += v3
    m32b += v4
    m32b += v5
    val m32 = m32b.finish
    
    val alpha = 4.235
    val beta = -99.759

    val ansVec = m23*colC
    collect(check(ansVec, Vector(168.2, 217.13)))

    // val ansVec2 = rowB*m32
    // collect(check(ansVec2, Vector(12676.229, 164.1640)))

    val a1 = m23*alpha
    // val a2 = a1 * (m33.t.inv)
    // val a3 = a2 * m32
    // val ansVec3 = a3 * (rowD.t*colE*beta)
    // collect(check(ansVec3, Vector(194179.526, 593097.843)))

    mkReport
  }
}


class SparseLinearAlgebraSuite extends DeliteSuite {
  def testSparseSimpleVector() { compileAndTest(SparseSimpleVectorArithmeticRunner) }
  def testSparseSimpleMatrix() { compileAndTest(SparseSimpleMatrixArithmeticRunner) }
  def testSparseCombinedVecMat() { compileAndTest(SparseCombinedVecMatArithmeticRunner) }
}

