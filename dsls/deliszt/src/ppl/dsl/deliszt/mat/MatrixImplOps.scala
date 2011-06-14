package ppl.dsl.deliszt.mat

import ppl.dsl.deliszt.datastruct.scala.{Vec,Mat,MatRow}
import ppl.dsl.deliszt.{DeLisztExp, DeLisztCompiler, DeLisztLift, DeLiszt}

trait MatrixImplOps { this: DeLisztExp =>
  def matrix_apply_impl[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
}

trait MatImplOpsStandard extends MatImplOps {
  this: DeLisztExp with DeLisztLift =>
}
