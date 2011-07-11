package ppl.dsl.deliszt.mat

import ppl.dsl.deliszt.datastruct.scala.{Vec,Mat,MatRow}
import ppl.dsl.deliszt.{DeLisztExp, DeLisztCompiler, DeLisztLift, DeLiszt}

trait MatImplOps { this: DeLisztExp =>
  def mat_apply_impl[R<:IntM,C<:IntM,A:Manifest](x: Rep[Mat[R,C,A]], i: Rep[Int], j: Rep[Int]): Rep[A]
}

trait MatImplOpsStandard extends MatImplOps {
  this: DeLisztExp with DeLisztLift =>
  
  def matrix_apply_impl[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]) = {
    val offset = i*x.numCols+j
    mat_dcapply(x, offset)
  }
}
