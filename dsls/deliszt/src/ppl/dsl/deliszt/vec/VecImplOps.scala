package ppl.dsl.deliszt.vec
import scala.virtualization.lms.common._

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import ppl.dsl.deliszt.{DeLisztLift, DeLisztCompiler, DeLiszt}

trait VecImplOps { this: DeLiszt =>
//  def vec_concatenate_impl[N<:IntM,VT:Manifest](v1: Rep[Vector[N,VT]], v2: Rep[Vector[N,VT]]): Rep[Vector[VT]]
  def vec_outer_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](v1: Rep[Vec[R,A]], v2: Rep[Vec[C,A]]): Rep[Mat[R,C,A]]
  def vec_normalize_impl[N<:IntM:Manifest:MVal,A:Manifest:Arith](x: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_cross_impl[A:Manifest:Arith](a: Rep[Vec[_3,A]], b: Rep[Vec[_3,A]]): Rep[Vec[_3,A]]
}

trait VecImplOpsStandard extends VecImplOps {
  this: DeLisztCompiler with DeLisztLift with MathOpsExp =>

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
  
  def vec_outer_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](collA: Rep[Vec[R,A]], collB: Rep[Vec[C,A]]) = {
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
  
  def vec_normalize_impl[N<:IntM:Manifest:MVal,A:Manifest:Arith](x: Rep[Vec[N,A]]) = {
    x / Math.sqrt((x * x).sum.AsInstanceOf[Double]).AsInstanceOf[A]
  }
  
  // Vector cross product for size 3 vector
  def vec_cross_impl[A:Manifest:Arith](a: Rep[Vec[_3,A]], b: Rep[Vec[_3,A]]) = {
    /*
    val out = Vec[_3,A](3)
    
    out(_0) = a(1) * b(2) - a(2) * b(1)
    out(_1) = a(2) * b(0) - a(0) * b(2)
    out(_2) = a(0) * b(1) - a(1) * b(0)
    
    out.unsafeImmutable
    */
    val x = a(1) * b(2) - a(2) * b(1)
    val y = a(2) * b(0) - a(0) * b(2)
    val z = a(0) * b(1) - a(1) * b(0)
    Vec(x,y,z)
  }
}
