package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Mat[R <: IntM, C <: IntM, @specialized(Boolean, Int, Long, Float, Double)VT] extends DeliteCollection[VT] {
  def apply[RR <: IntM, CC <: IntM](r : RR, c : CC)(implicit f : EnsureSize[RR,R], ff : EnsureSize[CC,C]) : VT
  def update[RR <: IntM, CC <: IntM](r : RR,c : CC, v : VT)(implicit f : EnsureSize[RR,R], f2 : EnsureSize[CC,C]) : Unit

  def apply(n : Int, m : Int) : VT = __
  def update(n : Int, m : Int, v : VT) : Unit = __

    // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit
}

trait VecView[@specialized(Boolean, Int, Long, Float, Double) VT] extends Vec[VT]

trait MatRow[@specialized(Boolean, Int, Long, Float, Double) VT] extends VecView[VT] {
  def index: Int
}
trait MatCol[@specialized(Boolean, Int, Long, Float, Double) VT] extends VecView[VT] {
  def index: Int
}