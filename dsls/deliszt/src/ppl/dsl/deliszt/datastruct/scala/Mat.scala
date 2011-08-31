package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection
import MetaInteger._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Mat[R<:IntM,C<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  def apply[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC) : T = apply(MIntDepth[RR], MIntDepth[CC])
  def update[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC, v : T) : Unit = update(MIntDepth[RR], MIntDepth[CC], v)

  def apply(n: Int, m: Int) : T
  def update(n: Int, m: Int, v : T) : Unit
  def size : Int
  
  def apply(idx: Int) : T
  def update(idx: Int, v: T) : Unit

  def row(n: Int) : MatRow[C,T]
  def col(n: Int) : MatCol[R,T]

  def numRows : Int
  def numCols : Int

    // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit
  def dcSize : Int = size
  
  def cloneL : Mat[R,C,T]
}

trait VecView[N<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends Vec[N,T]

trait MatRow[C<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends VecView[C,T]

trait MatCol[R<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends VecView[R,T]
