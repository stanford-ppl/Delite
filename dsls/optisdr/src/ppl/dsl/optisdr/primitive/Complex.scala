package ppl.dsl.optisdr.primitive

import scala.virtualization.lms.common._
import ppl.dsl.optisdr._

trait ComplexOps extends Variables {
  this: OptiSDR =>

  object Complex {
    // def apply(real: Rep[Real], imag: Rep[Real]) = complex_new(real, imag) 
  }
  
  implicit def repToComplexOps(x: Rep[Complex]) = new ComplexOpsCls(x)
  implicit def varToComplexOps(x: Var[Complex]) = new ComplexOpsCls(readVar(x))
  // implicit def ComplexToInterface[A:Manifest](lhs: Rep[DenseVector[A]]) = new VInterface[A](new DenseVecOpsCls[A](lhs))
  
  class ComplexOpsCls(x: Rep[Complex]) {
    // def conj
  }
}

trait ComplexOpsExp extends ComplexOps {
  this: OptiSDRExp =>
}