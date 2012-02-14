/* Testing implicit conversions for OptiML arithmetic operations.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jan 13, 2012
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.tests.scalatest.dsl.optiml

import ppl.dsl.optiml.{Vector,DenseVector,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.tests.scalatest._

object PrimitiveConversionsRunner extends DeliteTestRunner with OptiMLApplicationRunner with PrimitiveConversions
trait PrimitiveConversions extends DeliteTestModule with OptiMLApplication {
  def main() {
    val int = unit(1)
    val dbl = unit(1.0)    
    val flt = unit(1f)
    
    val lift1 = dbl + dbl
    val lift2 = dbl + int
    val lift3 = dbl + flt
    val lift4 = flt + flt
    val lift5 = flt + int
    val lift6 = flt + dbl
    val lift7 = int + int
    val lift8 = int + flt
    val lift9 = int + dbl
    collect(lift1 == 2 && lift1 == lift2 && lift2 == lift3 &&
            lift3 == lift4 && lift4 == lift5 && lift5 == lift6 &&
            lift6 == lift7 && lift7 == lift8 && lift8 == lift9)
    
    val lit1 = 1.0 + dbl
    val lit2 = 1.0 + int
    val lit3 = 1.0 + flt
    val lit4 = unit(1) /*TR FIXME*/ + dbl
    val lit5 = 1 + int
    val lit6 = 1 + flt
    val lit7 = unit(1f) /*TR FIXME*/ + dbl
    val lit8 = 1f + int
    val lit9 = 1f + flt
    val lit10 = dbl + 1.0
    val lit11 = int + 1.0
    val lit12 = flt + 1.0
    val lit13 = dbl + 1
    val lit14 = int + 1
    val lit15 = flt + 1
    val lit16 = dbl + 1f
    val lit17 = int + 1f
    val lit18 = flt + 1f
    collect(lit1 == 2 && lit1 == lit2 && lit2 == lit3 &&
            lit3 == lit4 && lit4 == lit5 && lit5 == lit6 &&
            lit6 == lit7 && lit7 == lit8 && lit8 == lit9 &&
            lit9 == lit10 && lit10 == lit11 && lit11 == lit12 &&
            lit12 == lit13 && lit13 == lit14 && lit14 == lit15 &&
            lit15 == lit16 && lit16 == lit17 && lit17 == lit18)
    

    mkReport          
  }
}

object VectorConversionsRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorConversions
trait VectorConversions extends DeliteTestModule with OptiMLApplication {
  def main() { 
    
    val vint = DenseVector[Int](100,true)
    val vdbl = DenseVector[Double](100,true)
    val vflt = DenseVector[Float](100,true)
    
    val int = unit(1)
    val dbl = unit(1.0)    
    val flt = unit(1f)
    
    val l1 = vint + vint
    val l2 = vint + vdbl
    val l3 = vint + vflt
    val l4 = vdbl + vint
    val l5 = vdbl + vflt
    val l6 = vdbl + vdbl
    val l7 = vflt + vint
    val l8 = vflt + vflt
    val l9 = vflt + vdbl
  
    val lift1 = int + vint
    val lift2 = int + vflt
    val lift3 = int + vdbl
    val lift4 = flt + vint    
    val lift5 = flt + vflt
    val lift6 = flt + vdbl
    val lift7 = dbl + vint
    val lift8 = dbl + vflt
    val lift9 = dbl + vdbl
    val lift10 = vint + int
    val lift11 = vflt + int
    val lift12 = vdbl + int
    val lift13 = vint + flt
    val lift14 = vflt + flt
    val lift15 = vdbl + flt
    val lift16 = vint + dbl
    val lift17 = vflt + dbl
    val lift18 = vdbl + dbl
    
    val lit1 = 1 + vint
    val lit2 = 1 + vflt
    val lit3 = 1 + vdbl
    val lit4 = 1f + vint
    val lit5 = 1f + vflt
    val lit6 = 1f + vdbl
    val lit7 = 1.0 + vint
    val lit8 = 1.0 + vflt
    val lit9 = 1.0 + vdbl
    val lit10 = vint + 1
    val lit11 = vflt + 1
    val lit12 = vdbl + 1
    val lit13 = vint + 1f
    val lit14 = vflt + 1f
    val lit15 = vdbl + 1f
    val lit16 = vint + 1.0
    val lit17 = vflt + 1.0
    val lit18 = vdbl + 1.0
    
    collect(true)
    mkReport
  }    
}

object MatrixConversionsRunner extends DeliteTestRunner with OptiMLApplicationRunner with MatrixConversions
trait MatrixConversions extends DeliteTestModule with OptiMLApplication {
  def main() { 
    val mint = DenseMatrix[Int](100,100)
    val mdbl = DenseMatrix[Double](100,100)
    val mflt = DenseMatrix[Float](100,100)
    
    val int = unit(1)
    val dbl = unit(1.0)    
    val flt = unit(1f)
    
    val l1 = mint + mint
    val l2 = mint + mdbl
    val l3 = mint + mflt
    val l4 = mdbl + mint
    val l5 = mdbl + mflt
    val l6 = mdbl + mdbl
    val l7 = mflt + mint
    val l8 = mflt + mflt
    val l9 = mflt + mdbl
  
    val lift1 = int + mint
    val lift2 = int + mflt
    val lift3 = int + mdbl
    val lift4 = flt + mint    
    val lift5 = flt + mflt
    val lift6 = flt + mdbl
    val lift7 = dbl + mint
    val lift8 = dbl + mflt
    val lift9 = dbl + mdbl
    val lift10 = mint + int
    val lift11 = mflt + int
    val lift12 = mdbl + int
    val lift13 = mint + flt
    val lift14 = mflt + flt
    val lift15 = mdbl + flt
    val lift16 = mint + dbl
    val lift17 = mflt + dbl
    val lift18 = mdbl + dbl
    
    val lit1 = 1 + mint
    val lit2 = 1 + mflt
    val lit3 = 1 + mdbl
    val lit4 = 1f + mint
    val lit5 = 1f + mflt
    val lit6 = 1f + mdbl
    val lit7 = 1.0 + mint
    val lit8 = 1.0 + mflt
    val lit9 = 1.0 + mdbl
    val lit10 = mint + 1
    val lit11 = mflt + 1
    val lit12 = mdbl + 1
    val lit13 = mint + 1f
    val lit14 = mflt + 1f
    val lit15 = mdbl + 1f
    val lit16 = mint + 1.0
    val lit17 = mflt + 1.0
    val lit18 = mdbl + 1.0
    
    collect(true)
    mkReport    
  }
}

class ArithmeticConversionSuite extends DeliteSuite {
  def testPrimitiveConversions() { compileAndTest(PrimitiveConversionsRunner) }
  def testVectorConversions() { compileAndTest(VectorConversionsRunner) }
  def testMatrixConversions() { compileAndTest(MatrixConversionsRunner) }
}

