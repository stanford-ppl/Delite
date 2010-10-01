package ppl.tests.standalone.simple_dsl

import ppl.delite.framework.{DeliteApplication, DSLType}

trait SimpleFloatMatrix extends DSLType { this: DeliteApplication =>

  case class MatrixZeros(n: Rep[Int]) extends Def[SimpleFloatMatrix]
  case class MatrixPlus(v1: Rep[SimpleFloatMatrix], v2: Rep[SimpleFloatMatrix]) extends Def[SimpleFloatMatrix]
  case class MatrixPPrint(v: Rep[SimpleFloatMatrix]) extends Def[String]

  def mzeros(n: Rep[Int]): Rep[SimpleFloatMatrix] = reflectEffect(MatrixZeros(n))
  def __ext__+(m1: Rep[SimpleFloatMatrix], m2: Rep[SimpleFloatMatrix])(implicit ef: MatrixErasureFix): Rep[SimpleFloatMatrix] = MatrixPlus(m1, m2)

  class SimpleFloatMatrixOps(m: Rep[SimpleFloatMatrix]) {
    def pprint: Rep[String] = MatrixPPrint(m)
  }

  //todo, need to be able to only import this stuff automatically
  implicit def injectOpsSFM(v:Rep[SimpleFloatMatrix]) = new SimpleFloatMatrixOps(v)

  //todo, cleanup these erasure fixes
  class MatrixErasureFix
  implicit val mef = new MatrixErasureFix
}