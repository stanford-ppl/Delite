package ppl.dsl.deliszt.vec

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztLift, DeLisztCompiler, DeLiszt}

trait VecImplOps { this: DeLiszt with MetaInteger =>
//  def vec_concatenate_impl[N<:IntM,VT:Manifest](v1: Rep[Vector[N,VT]], v2: Rep[Vector[N,VT]]): Rep[Vector[VT]]
  def vec_outer_impl[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](v1: Rep[Vec[R,A]], v2: Rep[Vec[C,A]]): Rep[Mat[R,C,A]]
}

trait VecImplOpsStandard extends VecImplOps {
  this: DeLisztCompiler with DeLisztLift with MetaInteger =>

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
  
  def vec_outer_impl[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](collA: Rep[Vec[R,A]], collB: Rep[Vec[C,A]]) = {
    val out = Mat[R,C,A](collA.size, collB.size)
    
    var i = 0
    while(i < collA.size) {
      var j = 0
      while(j < collB.size) {
        out(i,j) = collA(i)*collB(j)
        j += 1
      }
      i += 1
    }
    out.unsafeImmutable
  }
}