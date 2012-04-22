package ppl.dsl.deliszt.mat

import java.io.{PrintWriter}
import reflect.{Manifest, SourceContext}

import ppl.delite.framework.{DeliteApplication}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException}

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config

import ppl.dsl.deliszt.{DeLisztExp,DeLiszt}
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import ppl.dsl.deliszt.vec.VecOpsExp

import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.DeliteCollectionOpsExp

trait MatOps extends Variables {
  this: DeLiszt with LiftNumeric =>

  object Mat {
    def apply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest]() = mat_obj_n_new[R,C,A](MIntDepth[R], MIntDepth[C])
    def apply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](r: Rep[Int], c: Rep[Int]) = mat_obj_n_new[R,C,A](r,c)
    def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]]) = mat_obj_new[_1,N,VT](a1)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]]) = mat_obj_new[_2,N,VT](a1,a2)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]]) = mat_obj_new[_3,N,VT](a1,a2,a3)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]]) = mat_obj_new[_4,N,VT](a1,a2,a3,a4)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]]) = mat_obj_new[_5,N,VT](a1,a2,a3,a4,a5)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]]) = mat_obj_new[_6,N,VT](a1,a2,a3,a4,a5,a6)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]]) = mat_obj_new[_7,N,VT](a1,a2,a3,a4,a5,a6,a7)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]]) = mat_obj_new[_8,N,VT](a1,a2,a3,a4,a5,a6,a7,a8)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]]) = mat_obj_new[_9,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]]) = mat_obj_new[_10,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]]) = mat_obj_new[_11,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]]) = mat_obj_new[_12,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]]) = mat_obj_new[_13,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]]) = mat_obj_new[_14,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]]) = mat_obj_new[_15,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]]) = mat_obj_new[_16,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]],a17 : Rep[Vec[N,VT]]) = mat_obj_new[_17,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]],a17 : Rep[Vec[N,VT]],a18 : Rep[Vec[N,VT]]) = mat_obj_new[_18,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]],a17 : Rep[Vec[N,VT]],a18 : Rep[Vec[N,VT]],a19 : Rep[Vec[N,VT]]) = mat_obj_new[_19,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]],a17 : Rep[Vec[N,VT]],a18 : Rep[Vec[N,VT]],a19 : Rep[Vec[N,VT]],a20 : Rep[Vec[N,VT]]) = mat_obj_new[_20,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]],a17 : Rep[Vec[N,VT]],a18 : Rep[Vec[N,VT]],a19 : Rep[Vec[N,VT]],a20 : Rep[Vec[N,VT]],a21 : Rep[Vec[N,VT]]) = mat_obj_new[_21,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21)
		def apply[VT:Manifest,N<:IntM:Manifest:MVal](a1 : Rep[Vec[N,VT]],a2 : Rep[Vec[N,VT]],a3 : Rep[Vec[N,VT]],a4 : Rep[Vec[N,VT]],a5 : Rep[Vec[N,VT]],a6 : Rep[Vec[N,VT]],a7 : Rep[Vec[N,VT]],a8 : Rep[Vec[N,VT]],a9 : Rep[Vec[N,VT]],a10 : Rep[Vec[N,VT]],a11 : Rep[Vec[N,VT]],a12 : Rep[Vec[N,VT]],a13 : Rep[Vec[N,VT]],a14 : Rep[Vec[N,VT]],a15 : Rep[Vec[N,VT]],a16 : Rep[Vec[N,VT]],a17 : Rep[Vec[N,VT]],a18 : Rep[Vec[N,VT]],a19 : Rep[Vec[N,VT]],a20 : Rep[Vec[N,VT]],a21 : Rep[Vec[N,VT]],a22 : Rep[Vec[N,VT]]) = mat_obj_new[_22,N,VT](a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22)
  }

  implicit def repMatToMatOps[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Rep[Mat[R,C,A]]) = new matOpsCls(x)
  implicit def varToMatOps[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Var[Mat[R,C,A]]) = new matOpsCls(readVar(x))

  // could convert to infix, but apply doesn't work with it anyways yet
  class matOpsCls[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Rep[Mat[R,C,A]]) {
    type Self = Mat[R,C,A]

    // accessors
    def apply(r:Rep[Int],c:Rep[Int]) = mat_apply(x,r,c)
    def update(r:Rep[Int],c:Rep[Int],v:Rep[A]) = mat_update(x,r,c,v)

    def apply[RR <:IntM:Manifest:MVal ,CC <:IntM:Manifest:MVal](r:RR,c:CC) = mat_apply(x,MIntDepth[RR],MIntDepth[CC])
    def update[RR <:IntM:Manifest:MVal ,CC <:IntM:Manifest:MVal](r:RR,c:CC,v:Rep[A]) = mat_update(x,MIntDepth[RR],MIntDepth[CC],v)

    // Dims
    def numRows() : Rep[Int] = MIntDepth[R]
    def numCols() : Rep[Int] = MIntDepth[C]

    def t = mat_transpose(x)

    // arithmetic operations
    def +(y:Rep[Self])(implicit a:Arith[A]) = mat_plus(x,y)
    def -(y:Rep[Self])(implicit a:Arith[A]) = mat_minus(x,y)
    def unary_-(implicit a:Arith[A]) = mat_unary_minus(x)

    def *:*(y: Rep[Self])(implicit a: Arith[A]) = mat_times(x,y)
    def *[CC<:IntM:Manifest:MVal](y:Rep[Mat[C,CC,A]])(implicit a:Arith[A]) = mat_multiply(x,y)
    def *(y:Rep[Vec[C,A]])(implicit a:Arith[A],o:Overloaded1) = mat_times_vector(x,y)
    def *(y:Rep[A])(implicit a:Arith[A],o:Overloaded2) = mat_times_scalar(x,y)

    def /(y:Rep[Self])(implicit a:Arith[A]) = mat_zip_divide(x,y)
    def /(y:Rep[A])(implicit a:Arith[A],o:Overloaded1) = mat_divide_scalar(x,y)
    
    def cloneL = mat_clone(x)
    def mutable() = mat_mutable_clone(x)
  }

  def mat_obj_new[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](xs: Rep[Vec[C,A]]*):Rep[Mat[R,C,A]]
  def mat_obj_n_new[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](r: Rep[Int], c: Rep[Int]): Rep[Mat[R,C,A]]

  // class defs
  def mat_apply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Rep[Mat[R,C,A]],i:Rep[Int],j:Rep[Int]):Rep[A]
  def mat_update[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Rep[Mat[R,C,A]],i:Rep[Int],j:Rep[Int],y:Rep[A]):Rep[Unit]

  def mat_num_rows(m:Rep[Mat[_,_,_]]):Rep[Int]
  def mat_num_cols(m:Rep[Mat[_,_,_]]):Rep[Int]

  def row[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](m:Rep[Mat[R,C,A]],a:Rep[Int]):Rep[Vec[C,A]]
  def col[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](m:Rep[Mat[R,C,A]],a:Rep[Int]):Rep[Vec[R,A]]

  def mat_transpose[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]

  def mat_plus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_plus_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[A]):Rep[Mat[R,C,A]]
  def mat_minus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_times[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_multiply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,CC<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[C,CC,A]]):Rep[Mat[R,CC,A]]
  def mat_times_vector[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Vec[C,A]]):Rep[Vec[C,A]]
  def mat_times_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[A]):Rep[Mat[R,C,A]]
  def mat_divide_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[A]):Rep[Mat[R,C,A]]
  def mat_zip_divide[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_unary_minus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  
  def mat_mutable_clone[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Rep[Mat[R,C,A]]): Rep[Mat[R,C,A]]
  def mat_clone[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Rep[Mat[R,C,A]]): Rep[Mat[R,C,A]]
}


trait MatOpsExp extends MatOps with VariablesExp with DeliteCollectionOpsExp {
  this:MatImplOps with DeLisztExp with LiftNumeric =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class MatObjNew[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](vs: Exp[Vec[C,A]]*) extends Def[Mat[R,C,A]] {
    def r = manifest[R]
    def vr = implicitly[MVal[R]]
    def c = manifest[C]
    def vc = implicitly[MVal[C]]
    def a = manifest[A]
  }

  case class Mat3New[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](vs: Array[Exp[A]]) extends Def[Mat[R,C,A]] {
    def r = manifest[R]
    def vr = implicitly[MVal[R]]
    def c = manifest[C]
    def vc = implicitly[MVal[C]]
    def a = manifest[A]
  }
  
  case class MatObjNNew[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](numRows:Exp[Int],numCols:Exp[Int]) extends Def[Mat[R,C,A]] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }

  case class MatApply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int]) extends Def[A] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }

  case class MatDCApply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int]) extends Def[A] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }

  case class MatUpdate[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int],y:Exp[A]) extends Def[Unit] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }

  case class MatTranspose[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Exp[Mat[R,C,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_transpose_impl(x))) {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }
  
  case class MatGetRow[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Exp[Mat[R,C,A]], i: Exp[Int]) extends Def[MatRow[C,A]] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }

  case class MatGetCol[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Exp[Mat[R,C,A]], i: Exp[Int]) extends Def[MatCol[R,A]] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }
  
  case class MatNumRows(x:Exp[Mat[_,_,_]]) extends Def[Int]

  case class MatNumCols(x:Exp[Mat[_,_,_]]) extends Def[Int]

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines (currently these must all be singletasks)

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants

  case class MatTimesVec[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Vec[C,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_times_vector_impl(x,y)),true) {
    
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
    val aa = implicitly[Arith[A]]
  }

  case class MatMultiply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,CC<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[C,CC,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_multiply_impl(x,y)),true) {
    
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val cc = manifest[CC]
    val vcc = implicitly[MVal[CC]]
    val a = manifest[A]
    val aa = implicitly[Arith[A]]
  }
  
  case class MatClone[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal, A:Manifest](x: Exp[Mat[R,C,A]]) extends Def[Mat[R,C,A]] {
    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
  }

  ////////////////////////////////
  // implemented via delite ops 
  abstract class MatArithmeticMap[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](in:Exp[Mat[R,C,A]]) extends DeliteOpMap[A,A,Mat[R,C,A]] {
    def alloc = Mat[R,C,A](in.numRows, in.numCols)
    val size = in.numRows * in.numCols

    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
    val aa = implicitly[Arith[A]]
  }

  abstract class MatArithmeticZipWith[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]]) extends DeliteOpZipWith[A,A,A,Mat[R,C,A]] {
    def alloc = Mat[R,C,A](inA.numRows, inA.numCols)
    val size = inA.numRows * inA.numCols

    val r = manifest[R]
    val vr = implicitly[MVal[R]]
    val c = manifest[C]
    val vc = implicitly[MVal[C]]
    val a = manifest[A]
    val aa = implicitly[Arith[A]]
  }

  case class MatPlus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a + b
  }

  case class MatMinus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a - b
  }

  case class MatUnaryMinus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](in:Exp[Mat[R,C,A]])
    extends MatArithmeticMap(in) {

    def func = e => -e
  }

  case class MatTimes[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a * b
  }
  
  case class MatTimesScalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](in:Exp[Mat[R,C,A]],y:Exp[A])
    extends MatArithmeticMap(in) {

    def func = e => e * y
  }

  case class MatDivide[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a / b
  }
  
  case class MatDivideScalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](in:Exp[Mat[R,C,A]],y:Exp[A])
    extends MatArithmeticMap(in) {

    def func = e => e / y
  }
  
  case class MatPlusScalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](in:Exp[Mat[R,C,A]],y:Exp[A])
    extends MatArithmeticMap(in) {

    def func = e => e + y
  }
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    (e match {
      case e@Mat3New(xs) => reflectPure(Mat3New(xs.map(z=>f(z)))(e.r, e.vr, e.c, e.vc, e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatObjNew(xs @ _*) => reflectPure(MatObjNew(f(xs) : _*)(e.r, e.vr, e.c, e.vc, e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatApply(x,i,j) => mat_apply(f(x), f(i), f(j))(e.r, e.vr, e.c, e.vc, e.a)
      case e@MatGetRow(x,i) => row(f(x),f(i))(e.r, e.vr, e.c, e.vc, e.a)
      case e@MatGetCol(x,i) => col(f(x),f(i))(e.r, e.vr, e.c, e.vc, e.a)
      case e@MatMultiply(x,y) => reflectPure(new { override val original = Some(f,e) } with MatMultiply(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.cc, e.vcc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with MatTimes(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatTimesScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatTimesScalar(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatTimesVec(x,y) => reflectPure(new { override val original = Some(f,e) } with MatTimesVec(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatDivide(x,y) => reflectPure(new { override val original = Some(f,e) } with MatDivide(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatDivideScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatDivideScalar(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatPlus(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatPlusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatPlusScalar(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatMinus(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatUnaryMinus(x) => reflectPure(new { override val original = Some(f,e) } with MatUnaryMinus(f(x))(e.r, e.vr, e.c, e.vc, e.a, e.aa))(mtype(manifest[A]),ctx)
      case e@MatTranspose(x) => reflectPure(new { override val original = Some(f,e) } with MatTranspose(f(x))(e.r, e.vr, e.c, e.vc, e.a))(mtype(manifest[A]),ctx)
      case Reflect(e@MatMultiply(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatMultiply(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.cc, e.vcc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatTimes(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatTimes(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatTimesScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatTimesScalar(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatTimesVec(x,y), u, es)  => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatTimesVec(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatDivide(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatDivide(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatDivideScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatDivideScalar(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatPlus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatPlus(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatPlusScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatPlusScalar(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatMinus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatMinus(f(x),f(y))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatUnaryMinus(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatUnaryMinus(f(x))(e.r, e.vr, e.c, e.vc, e.a, e.aa), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatTranspose(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatTranspose(f(x))(e.r, e.vr, e.c, e.vc, e.a), mapOver(f,u), f(es)))(mtype(manifest[A])) 
      case MatNumRows(x) => mat_num_rows(f(x))
      case MatNumCols(x) => mat_num_cols(f(x))
      // Read/write effects
      case Reflect(e@MatApply(x,i,j), u, es) => reflectMirrored(Reflect(MatApply(f(x),f(i),f(j))(e.r, e.vr, e.c, e.vc, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatGetRow(x,i), u, es) => reflectMirrored(Reflect(MatGetRow(f(x),f(i))(e.r, e.vr, e.c, e.vc, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatUpdate(x,i,j,r), u, es) => reflectMirrored(Reflect(MatUpdate(f(x),f(i),f(j),f(r))(e.r, e.vr, e.c, e.vc, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
      // Effect with SingleTask and DeliteOpLoop
      // Allocation
      case Reflect(e@MatObjNew(xs @ _*), u, es) => reflectMirrored(Reflect(MatObjNew(f(xs) : _*)(e.r, e.vr, e.c, e.vc, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatObjNNew(i,j), u, es) => reflectMirrored(Reflect(MatObjNNew(f(i),f(j))(e.r, e.vr, e.c, e.vc, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Mat3New(a) => a.flatMap(syms).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Mat3New(a) => a.flatMap(symsFreq).toList
    case _ => super.symsFreq(e)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case MatClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case MatClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case MatClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case MatClone(a) => syms(a)
    case _ => super.copySyms(e)
  } 

  ////////////////////
  // object interface
  def mat_obj_new[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](vs: Exp[Vec[C,A]]*): Exp[Mat[R,C,A]] = {    
    MatObjNew[R,C,A](vs:_*)
  }
  
  def mat_obj_n_new[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](r: Exp[Int], c: Exp[Int]) = {
    reflectMutable(MatObjNNew[R,C,A](r,c))//.unsafeImmutable
  }

  ///////////////////
  // class interface
  
  def mat_num_rows(x:Exp[Mat[_,_,_]]) = reflectPure(MatNumRows(x))
  def mat_num_cols(x:Exp[Mat[_,_,_]]) = reflectPure(MatNumCols(x))
  
  def row[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](m:Exp[Mat[R,C,A]],a:Exp[Int]) = reflectPure(MatGetRow(m,a))
  def col[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](m:Exp[Mat[R,C,A]],a:Exp[Int]) = reflectPure(MatGetCol(m,a))

  def mat_apply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int]) = reflectPure(MatApply(x,i,j))
  def mat_update[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int],y:Exp[A]) = reflectWrite(x)(MatUpdate(x,i,j,y))

  def mat_transpose[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Rep[Mat[R,C,A]]) = reflectPure(MatTranspose(x))

  def mat_plus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatPlus(x,y))
  def mat_plus_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[A]) = reflectPure(MatPlusScalar(x,y))
  def mat_minus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatMinus(x,y))
  def mat_unary_minus[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]]) = MatUnaryMinus(x)

  def mat_times[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatTimes(x,y))
  def mat_multiply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,CC<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[C,CC,A]]) = reflectPure(MatMultiply(x,y))
  def mat_times_vector[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Vec[C,A]]) = reflectPure(MatTimesVec(x,y))
  def mat_times_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[A]) = reflectPure(MatTimesScalar(x,y))

  def mat_zip_divide[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatDivide(x,y))
  def mat_divide_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[A]) = reflectPure(MatDivideScalar(x,y))

  //////////////////
  // internal

  def mat_dcapply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int]) = reflectPure(MatDCApply(x,i))
  
  def mat_mutable_clone[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Exp[Mat[R,C,A]]) = reflectMutable(MatClone(x))
  def mat_clone[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](x: Exp[Mat[R,C,A]]) = reflectPure(MatClone(x))
}

/**
 *  Optimizations for composite MatOps operations.
 */

trait MatOpsExpOpt extends MatOpsExp {
  this:MatImplOps with DeLisztExp with LiftNumeric =>

  override def mat_obj_new[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest](vs: Exp[Vec[C,A]]*): Exp[Mat[R,C,A]] = {    
    if (vs.length == 3) {
      printdbg("!!! found a matrix constructor with Vec3 args !!!")
      val buf = new scala.collection.mutable.ArrayBuffer[Exp[A]]()
      vs foreach { e => e match {
        case Def(Vec3New(a,b,c)) => buf += a; buf += b; buf += c
        
        // could also rewrite the vec3 delite op operations to return VecNew nodes instead..        
        case Def(m: DeliteOpMap[A,A,_]) => m.body match {
          case ce: DeliteCollectElem[_,_] => ce.alloc match {
            case Def(Vec3New(a,b,c)) => 
              buf ++= (0 to 2) map { i => reifyEffects(m.func(dc_apply(m.in.asInstanceOf[Exp[DeliteCollection[A]]],unit(i))).asInstanceOf[Exp[A]]).res }
            case Def(Reflect(Vec3New(a,b,c), u, es))  =>
              buf ++= (0 to 2) map { i => reifyEffects(m.func(dc_apply(m.in.asInstanceOf[Exp[DeliteCollection[A]]],unit(i))).asInstanceOf[Exp[A]]).res }
            case Def(Reify(Def(Reflect(Vec3New(a,b,c), u, es)), _,_)) => 
              buf ++= (0 to 2) map { i => reifyEffects(m.func(dc_apply(m.in.asInstanceOf[Exp[DeliteCollection[A]]],unit(i))).asInstanceOf[Exp[A]]).res }
            case _ => printdbg(" XXXXXXXXXXXXXXXXXXXXXXX found non vec3?! : " + ce.alloc.Type.toString)
                      printdbg(" XXXXXXXXXXXXXXXXXXXXXXX def is: " + findDefinition(ce.alloc.res.asInstanceOf[Sym[Any]]).toString)
           }
        }        
        case Def(z: DeliteOpZipWith[A,A,_,_]) => z.body match {
          case ce: DeliteCollectElem[_,_] => ce.alloc match {
            case Def(Vec3New(a,b,c)) =>
              buf ++= (0 to 2) map { i => reifyEffects(z.func(dc_apply(z.inA.asInstanceOf[Exp[DeliteCollection[A]]],unit(i)),dc_apply(z.inB.asInstanceOf[Exp[DeliteCollection[A]]],unit(i))).asInstanceOf[Exp[A]]).res }
            case Def(Reify(Def(Reflect(Vec3New(a,b,c), u, es)), _, _)) =>
              buf ++= (0 to 2) map { i => reifyEffects(z.func(dc_apply(z.inA.asInstanceOf[Exp[DeliteCollection[A]]],unit(i)),dc_apply(z.inB.asInstanceOf[Exp[DeliteCollection[A]]],unit(i))).asInstanceOf[Exp[A]]).res }
           case _ => printdbg(" XXXXXXXXXXXXXXXXXXXXXXX found non vec3?! : " + ce.alloc.Type.toString)
                     printdbg(" XXXXXXXXXXXXXXXXXXXXXXX def is: " + findDefinition(ce.alloc.res.asInstanceOf[Sym[Any]]).toString)
	  }
        }            
        // case Def(e: DeliteOpLoop[_]) => e.body match {
        //           case ce: DeliteCollectElem[_,_] => ce.alloc match {
        //             case Def(Vec3New(a,b,c)) => buf += a.asInstanceOf[Exp[A]]; buf += b.asInstanceOf[Exp[A]]; buf += c.asInstanceOf[Exp[A]]            
        //           }
        //         }
        case _ => printdbg(" XXXXXXXXXXXXXXXXXXXXXXX found non vec3?! : " + e.Type.toString)
                  printdbg(" XXXXXXXXXXXXXXXXXXXXXXX def is: " + findDefinition(e.asInstanceOf[Sym[Any]]).toString)
      }}
      //if (buf.length == 0) return reflectMutable(MatObjNew[R,C,A](vs:_*))//.unsafeImmutable 
      //else return reflectMutable(Mat3New[R,C,A](buf.toArray))//.unsafeImmutable
      if (buf.length == 0) return MatObjNew[R,C,A](vs:_*)
      else return Mat3New[R,C,A](buf.toArray)
    }
    else 
      super.mat_obj_new(vs: _*)(manifest[R],implicitly[MVal[R]],manifest[C],implicitly[MVal[C]],manifest[A]) 
  }
}


trait ScalaGenMatOps extends ScalaGenBase {
  val IR:MatOpsExp with VecOpsExp

  import IR._
  
  val matImplPath = "ppl.dsl.deliszt.datastruct.scala.MatImpl"  
  val mat3x3ImplPath = "ppl.dsl.deliszt.datastruct.scala.Mat3x3Impl"

  override def emitNode(sym:Sym[Any],rhs:Def[Any])(implicit stream:PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@MatObjNew(vs @ _*) => emitValDef(sym, remap(matImplPath, "", m.a) + "(" + vs.map(quote).reduceLeft(_+","+_) + ")")
    case m@Mat3New(xs) => emitValDef(sym, " new " + remap(mat3x3ImplPath, "", m.a) + "(" + xs.map(quote).mkString(",") + ")")
    case m@MatObjNNew(numRows,numCols) => emitValDef(sym, remap(matImplPath, ".ofSize", m.a) + "(" + quote(numRows) + "," + quote(numCols) + ")")
    //case MatApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    case MatDCApply(x,i) => emitValDef(sym,quote(x) + ".dcApply(" + quote(i) + ")")
    case MatApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    case MatUpdate(x,i,j,y) => emitValDef(sym,quote(x) + "(" + quote(i) + ", " + quote(j) + ") = " + quote(y))

    case MatGetRow(x,i) => emitValDef(sym,quote(x) + ".row(" + quote(i) + ")")
    case MatGetCol(x,i) => emitValDef(sym,quote(x) + ".col(" + quote(i) + ")")
    
    case MatNumRows(x) => emitValDef(sym,quote(x) + ".numRows")
    case MatNumCols(x) => emitValDef(sym,quote(x) + ".numCols")
    
    case MatClone(x) => emitValDef(sym, quote(x) + ".cloneL")

    // BLAS calls
    // all corresponding nodes should have their DeliteOpSingleTask second argument set to "true" (require inputs)
    case m@MatMultiply(x,y) if (Config.useBlas) =>
      emitValDef(sym,"generated.scala.Mat[" + remap(m.a) + "](" + quote(x) + ".numRows," + quote(y) + ".numCols)")
      stream.println("scalaBLAS.matMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,%s.numCols)".format(quote(x),quote(y),quote(sym),quote(x),quote(x),quote(y)))
    case m@MatTimesVec(x,y) if (Config.useBlas) =>
      emitValDef(sym,"generated.scala.Mat[" + remap(m.a) + "](" + quote(x) + ".numRows, false)")
      stream.println("scalaBLAS.matVMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,0,1)".format(quote(x),quote(y),quote(sym),quote(x),quote(x)))
      stream.println("scalaBLAS.sigmoid(%s.data,%s.data,0,%s.numRows*%s.numCols)".format(quote(x),quote(sym),quote(x),quote(x)))
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenMatOps extends CudaGenBase {
  val IR:MatOpsExp

  import IR._

  override def emitNode(sym:Sym[Any],rhs:Def[Any])(implicit stream:PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@MatObjNew(vs @ _*) if(!isHostAlloc) => emitValDef(sym, remap(sym.Type) + "()")
                                                 vs.zipWithIndex.foreach(elem => stream.println("%s.vectorUpdate(%s, %s);".format(quote(sym),elem._2,quote(elem._1))))
    case m@Mat3New(xs) if(!isHostAlloc) => emitValDef(sym, remap(sym.Type) + "()")
                                                 xs.zipWithIndex.foreach(elem => stream.println("%s.dcUpdate(%s, %s);".format(quote(sym),elem._2,quote(elem._1))))
    case m@MatObjNNew(numRows,numCols) if(!isHostAlloc) => emitValDef(sym, "Mat<" + remap(m.a) + "," + quote(numRows) + "," + quote(numCols) + ">()")
    case MatDCApply(x,i) => emitValDef(sym,quote(x) + ".dcApply(" + quote(i) + ")")
    case MatApply(x,i,j) => emitValDef(sym, quote(x) + ".apply(" + quote(i) + ", " + quote(j) + ")")
    case MatUpdate(x,i,j,y) => stream.println(quote(x) + ".update(" + quote(i) + ", " + quote(j) + "," + quote(y) + ");")

    case MatGetRow(x,i) => emitValDef(sym,quote(x) + ".row(" + quote(i) + ")")
    case MatGetCol(x,i) => emitValDef(sym,quote(x) + ".col(" + quote(i) + ")")
    
    case MatNumRows(x) => emitValDef(sym,quote(x) + ".numRows")
    case MatNumCols(x) => emitValDef(sym,quote(x) + ".numCols")
    
    case MatClone(x) => emitValDef(sym, quote(x))
    
    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenMatOps extends CGenBase {
  val IR:MatOpsExp

  import IR._

  override def emitNode(sym:Sym[Any],rhs:Def[Any])(implicit stream:PrintWriter) = rhs match {
    case _ => super.emitNode(sym,rhs)
  }
}
