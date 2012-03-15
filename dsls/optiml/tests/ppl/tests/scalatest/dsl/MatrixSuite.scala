/* Unit tests for OptiML matrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 22, 2010
 * modified: Mar 31, 2011
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.tests.scalatest.dsl.optiml

import ppl.dsl.optiml.{Vector,RangeVector}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.tests.scalatest._

object MatrixAccessorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with MatrixAccessors
trait MatrixAccessors extends DeliteTestModule with OptiMLApplication {
  def main() {
    val m = Matrix.rand(100,100)

    collect(m.numRows == 100)
    collect(m.numCols == 100)
    //collect(m.size == m.numRows*m.numCols)

    val elem = m(92,10)
    collect(m(92,10) == elem)

    val row = m(37)
    collect(row.length == m.numCols)
    var j = 0
    while (j < m.numCols){
      collect(row(j) == m(37,j))
      //collect(row(j) == m.flattened(37*m.numCols+j))
      j += 1
    }

    val col = m.getCol(52)
    collect(col.length == m.numRows)
    j = 0
    while (j < m.numRows){
      collect(col(j) == m(j,52))
      //collect(col(j) == m.flattened(j*m.numCols+52))
      j += 1
    }

    val mat = m.sliceRows(3,5)
    var i = 0;
    j = 0
    while (i < mat.numRows){
      while (j < mat.numCols){
        collect(mat(i,j) == m(i+3,j))
        j += 1
      }
      i += 1
    }

    mkReport
  }
}

object MatrixOperatorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with MatrixOperators
trait MatrixOperators extends DeliteTestModule with OptiMLApplication {
  def main() {
    val m_rand = Matrix.rand(2,2)
    collect(m_rand(0,0) != m_rand(0,1))
    collect(m_rand(0,0) != m_rand(1,0))
    collect(m_rand(0,0) != m_rand(1,1))

    mkReport
  }
}

object MatrixUpdatesRunner extends DeliteTestRunner with OptiMLApplicationRunner with MatrixUpdates
trait MatrixUpdates extends DeliteTestModule with OptiMLApplication {
  def main() {
    val v = Vector.rand(100)
    val m = Matrix.rand(100,100).mutable
    val mb = Matrix.rand(100,100).mutable

    val init_m = m.Clone

    m(72,5) = 3.14
    collect(m(72,5) == 3.14)

    m(3) = v
    var j = 0
    while (j < m.numCols){
      collect(v(j) == m(3,j))
      j += 1
    }

    var rows = m.numRows
    m.insertRow(6,v)
    collect(m.numRows == rows+1)
    j = 0
    while (j < m.numCols){
      collect(m(6,j) == v(j))
      j += 1
    }
    rows += 1

    m.insertAllRows(72,mb)
    collect(m.numRows == rows+mb.numRows)
    var i = 0
    j = 0
    while (i < mb.numRows){
      while (j < mb.numCols){
        collect(m(i+72,j) == mb(i,j))
        j += 1
      }
      i += 1
    }
    rows += mb.numRows

    val m2 = init_m.mutable
    rows = m2.numRows
    var cols = m.numCols
    m2.insertCol(17,v)
    collect(m2.numCols == cols+1)
    j = 0
    while (j < m2.numRows){
      collect(m2(j,17) == v(j))
      j += 1
    }
    cols += 1

    m2.insertAllCols(99,mb)
    collect(m2.numCols == cols+mb.numCols)
    i = 0
    j = 0
    while (i < mb.numRows){
      while (j < mb.numCols){
        collect(m2(i,j+99) == mb(i,j))
        j += 1
      }
      i += 1
    }
    cols += mb.numCols

    val s_row = m2(20).Clone
    m2.removeRows(10,10)
    collect(m2.numRows == rows-10)
    //collect(s_row.cmp(m(10)).value == true)
    rows -= 10

    val s_col = m2.getCol(23).Clone
    m2.removeCols(13,10)
    collect(m2.numCols == cols-10)
    //collect(s_col.cmp(m.getCol(13)).value == true)
    cols -= 10

    mkReport
  }
}

object GroupRowsByRunner extends DeliteTestRunner with OptiMLApplicationRunner with GroupRowsBy
trait GroupRowsBy extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val m = Matrix(Vector(1,2,3,4),
                   Vector(2,-2,-3,-4),
                   Vector(1,5,6,7),
                   Vector(2,-5,-6,-7))
    
    val ms = m.groupRowsBy(row => row(0))
    collect(ms.length == 2)
    for (m <- ms) {
      m.pprint
      collect(m.numRows == 2)
      collect(m.numCols == 4) 
    }
    mkReport
  }
}

class MatrixSuite extends DeliteSuite {
  def testAccessors() { compileAndTest(MatrixAccessorsRunner) }
  def testOperators() { compileAndTest(MatrixOperatorsRunner) }
  def testUpdates() { compileAndTest(MatrixUpdatesRunner) }
  def testGroupRowsBy() { compileAndTest(GroupRowsByRunner) }
}