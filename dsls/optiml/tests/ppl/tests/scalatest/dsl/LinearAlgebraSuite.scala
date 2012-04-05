package ppl.tests.scalatest.dsl.optiml

import ppl.dsl.optiml._
import scala.virtualization.lms.util.OverloadHack
import ppl.tests.scalatest._

object SimpleVectorArithmeticRunner extends DeliteTestRunner with OptiMLApplicationRunner with SimpleVectorArithmetic
trait SimpleVectorArithmetic extends DeliteTestModule with LinearAlgebraTestsCommon {
  def main() = {
    // TODO: these can't be factored out right now because they throw an NPE when the test is being initialized
    val rowA = Vector(11., 22., 33.)
    val rowB = Vector(-5.3, -17.2, -131.)
    val rowD = Vector(-1.1, -6.2)
    val colC = Vector(7., 3.2, 13.3).t

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

object SimpleMatrixArithmeticRunner extends DeliteTestRunner with OptiMLApplicationRunner with SimpleMatrixArithmetic
trait SimpleMatrixArithmetic extends DeliteTestModule with LinearAlgebraTestsCommon {
  def main() = {
    val rowA = Vector(11., 22., 33.)
    val rowB = Vector(-5.3, -17.2, -131.)
    val colC = Vector(7., 3.2, 13.3).t
    val m33 = Matrix(rowA, rowB, colC.t)
    val m23 = Matrix(Vector(3.5, 7.5, 9.0), Vector(-5.6, 8.2, 17.3))
    val m32 = Matrix(Vector(.07, .91), Vector(17., -10.), Vector(-99.,.023))

    // matrix square multiplication
    val ansMat = m33*m33
    collect(check(ansMat, Matrix(Vector(235.4, -30.8, -2080.1), Vector(-884.14, -239.96, 336.),
                                 Vector(153.14, 141.52, -11.31))))

    // inverse
    val ansMat2 = m33.inv
    collect(check(ansMat2, Matrix(Vector(-.0145, 0.0143, 0.1765), Vector(0.0645, 0.0065, -0.0965),
                                 Vector(-0.0079, -0.0091, 0.0055))))

    // matrix transpose
    val ansMat3 = m33.t
    collect(check(ansMat3, Matrix(Vector(11., -5.3, 7.), Vector(22., -17.2, 3.2), Vector(33., -131., 13.3))))

    // matrix multiplication
    val ansMat4 = m33*m32
    collect(check(ansMat4, Matrix(Vector(-2892.223, -209.2310), Vector(12676.229, 164.1640), Vector(-1261.81, -25.3241))))

    // chained matrix multiplication
    val ansMat5 = m23*m33*m32
    collect(check(ansMat5, Matrix( Vector(73592.6225, 271.0046), Vector(98312.252799, 2079.73147))))

    // summation
    val ans = m23.sum
    collect(check(ans, 39.9))

    mkReport
  }
}

object CombinedVecMatArithmeticRunner extends DeliteTestRunner with OptiMLApplicationRunner with CombinedVecMatArithmetic
trait CombinedVecMatArithmetic extends DeliteTestModule with LinearAlgebraTestsCommon {
  def main() = {
    val rowA = Vector(11., 22., 33.)
    val rowB = Vector(-5.3, -17.2, -131.)
    val rowD = Vector(-1.1, -6.2)
    val colC = Vector(7., 3.2, 13.3).t
    val colE = Vector(.05, 9.97).t
    val m33 = Matrix(rowA, rowB, colC.t)
    val m23 = Matrix(Vector(3.5, 7.5, 9.0), Vector(-5.6, 8.2, 17.3))
    val m32 = Matrix(Vector(.07, .91), Vector(17., -10.), Vector(-99.,.023))
    val alpha = 4.235
    val beta = -99.759

    val ansVec = m23*colC
    collect(check(ansVec, Vector(168.2, 217.13)))

    val ansVec2 = rowB*m32
    collect(check(ansVec2, Vector(12676.229, 164.1640)))

    val a1 = m23*alpha
    val a2 = a1 * (m33.t.inv)
    val a3 = a2 * m32
    val ansVec3 = a3 * (rowD.t*colE*beta)
    collect(check(ansVec3, Vector(194179.526, 593097.843)))

    mkReport
  }
}

trait LinearAlgebraTestsCommon extends OptiMLApplication with OverloadHack {
  this: DeliteTestModule =>

  ////////////////
  // helpers

  def approx(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = {
    // be very generous w.r.t. precision, because the ground truth
    // answers have not all been entered with high precision
    abs(x - y) < .01
  }

  def check(x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]): Rep[Boolean] = {
    if (x.length != y.length) {
      false
    }
    else {
      val res = x.zip(y) { (a,b) => approx(a,b) }
      if ((res count { _ == false }) > 0) false
      else true
    }
  }

  // if we use Overloaded1 here, we get an ambiguous implicit inside Equal; we need to separate
  // the Overloaded "namespaces" from Application and Framework to be able to use this trick consistently
  def check(x: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]])(implicit o: Overloaded9): Rep[Boolean] = {
    if ((x.numRows != y.numRows) || (x.numCols != y.numCols)) {
      false
    }
    else {
      val res = x.zip(y) { (a,b) => approx(a,b) }
      if ((res count { _ == false }) > 0) false
      else true
    }
  }

  def check(x: Rep[Double], y: Rep[Double])(implicit o: Overloaded2): Rep[Boolean] = {
    approx(x,y)
  }
}


class LinearAlgebraSuite extends DeliteSuite {
  def testSimpleVector() { compileAndTest(SimpleVectorArithmeticRunner) }
  def testSimpleMatrix() { compileAndTest(SimpleMatrixArithmeticRunner) }
  def testCombinedVecMat() { compileAndTest(CombinedVecMatArithmeticRunner) }
}

