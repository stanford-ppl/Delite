package ppl.dsl.deliszt.mat

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import ppl.dsl.deliszt.{DeLisztExp, DeLisztCompiler, DeLisztLift, DeLiszt}

trait MatImplOps { this: DeLisztExp =>
  def mat_apply_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Rep[Mat[R,C,A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def mat_transpose_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](m: Rep[Mat[R,C,A]]): Rep[Mat[R,C,A]]
  def mat_multiply_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,CC<:IntM:Manifest:MVal,A:Manifest:Arith](x: Rep[Mat[R,C,A]], y: Rep[Mat[C,CC,A]]): Rep[Mat[R,CC,A]]
  def mat_times_vector_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x: Rep[Mat[R,C,A]], y: Rep[Vec[C,A]]): Rep[Vec[C,A]]
}

trait MatImplOpsStandard extends MatImplOps {
  this: DeLisztExp with DeLisztLift =>
  
  def mat_apply_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Rep[Mat[R,C,A]], i: Rep[Int], j: Rep[Int]) = {
    val offset = i*x.numCols+j
    mat_dcapply(x, offset)
  }
  
  def mat_transpose_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](m: Rep[Mat[R,C,A]]) = {
    // naive, should block
    val out = Mat[R,C,A](m.numRows,m.numCols)
    for (i <- 0 until out.numRows){
      for (j <- 0 until out.numCols){
        out(i,j) = m(j,i)
      }
    }
    out.unsafeImmutable
  }
  
  def mat_multiply_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,CC<:IntM:Manifest:MVal,A:Manifest:Arith](x: Rep[Mat[R,C,A]], y: Rep[Mat[C,CC,A]]): Rep[Mat[R,CC,A]] = {
    val yTrans = y.t
    val out = Mat[R,CC,A](x.numRows,y.numCols)

    var rowIdx = 0
    while (rowIdx < x.numRows) {
      var i = unit(0)
      while (i < out.numCols) {
        var j = unit(1)
        var acc = x(rowIdx, 0) * yTrans(i, 0)
        while (j < yTrans.numCols) {
          acc += x(rowIdx, j) * yTrans(i, j)
          j += 1
        }
        out(rowIdx, i) = acc
        i += 1
      }
      rowIdx += 1
    } 
    out.unsafeImmutable
  }
  
  def mat_times_vector_impl[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x: Rep[Mat[R,C,A]], y: Rep[Vec[C,A]]): Rep[Vec[C,A]] = {
    val out = Vec[C,A](y.size)

    var rowIdx = 0
    while (rowIdx < x.numRows) {
      out(rowIdx) = dot(row(x, rowIdx), y)
      rowIdx += 1
    }
    out.unsafeImmutable
  } 
}
