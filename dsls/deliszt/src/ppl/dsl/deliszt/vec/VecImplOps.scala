package ppl.dsl.deliszt.vec

import ppl.dsl.deliszt.datastruct.scala.{Vec,Mat}
import ppl.dsl.deliszt.{DeLisztLift, DeLisztCompiler, DeLiszt}

trait VecImplOps { this: DeLiszt =>
//  def vec_concatenate_impl[N<:IntM,VT:Manifest](v1: Rep[Vector[N,VT]], v2: Rep[Vector[N,VT]]): Rep[Vector[VT]]
}

trait VecImplOpsStandard extends VecImplOps {
  this: DeLisztCompiler with DeLisztLift =>

/*  def vec_concatenate_impl[VT:Manifest](v1: Rep[Vector[VT]], v2: Rep[Vector[VT]]) = {
    val out = Vector[VT](v1.length+v2.length, v1.isRow)
    for (i <- 0 until v1.length){
      out(i) = v1(i)
    }
    for (i <- 0 until v2.length){
      out(i+v1.length) = v2(i)
    }
    out
  } */
}