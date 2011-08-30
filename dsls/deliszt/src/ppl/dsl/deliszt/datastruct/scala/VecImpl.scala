package ppl.dsl.deliszt.datastruct.scala

import MetaInteger._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object VecImpl {
  def apply[N<:IntM:MVal, T:ClassManifest]() = {
    new VecImpl[N,T](MIntDepth[N])
  }
  
  def apply[N<:IntM:MVal, T:ClassManifest](xs: T*) = {
    new VecImpl[N,T](xs.toArray)
  }
  
  def apply[N<:IntM:MVal, T:ClassManifest](data : Array[T]) = {
    new VecImpl[N,T](data)
  }
}

class VecImpl[N<:IntM:MVal, @specialized T: ClassManifest](val data : Array[T]) extends Vec[N, T] with Copyable {
  def this(size : Int) = this(new Array[T](size))
  
  def size = data.length

  def apply[TT<:IntM](n : TT)(implicit mv: MVal[TT], f : EnsureSize[TT,N]) : T = apply(MIntDepth[TT])
  def update[TT<:IntM](n : TT, v : T)(implicit mv: MVal[TT], f : EnsureSize[TT,N]) : Unit = update(MIntDepth[TT], v)

  def apply(n : Int) = data(n)
  def update(n : Int, v : T) = {
    data(n) = v
  }
  
  def copy() = {
    val v = new VecImpl(size)
  
    if(classManifest[T] <:< classManifest[Copyable]) {
      for(i <- 0 until size) {
        v(i) = data(i).asInstanceOf[Copyable].copy.asInstanceOf[T]
      }
    }
    else {
      for(i <- 0 until size) {
        v(i) = data(i)
      }
    }
    
    v
  }
}
