package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Vec[N <: IntM, VT] {
  type Self = Vec[N,VT]
  def x(implicit f : EnsureSize[_0,N]) : VT
  def y(implicit f : EnsureSize[_1,N]) : VT
  def z(implicit f : EnsureSize[_2,N]) : VT
  def w(implicit f : EnsureSize[_3,N]) : VT
  def apply[TT <: IntM](n : TT)(implicit f : EnsureSize[TT,N]) : VT
  def +(vt : Self)(implicit f : Numeric[VT]) : Vec[N,VT]
  def -(vt : Self)(implicit f : Numeric[VT]) : Vec[N,VT]
  def *(vt : Self)(implicit f : Numeric[VT]) : Vec[N,VT]
  def /(vt : Self)(implicit f : Numeric[VT]) : Vec[N,VT]
  def *(vt : VT)(implicit f : Numeric[VT]) : Vec[N,VT]
  def /(vt : VT)(implicit f : Numeric[VT]) : Vec[N,VT]
  def unary_- : Vec[N,VT]
}