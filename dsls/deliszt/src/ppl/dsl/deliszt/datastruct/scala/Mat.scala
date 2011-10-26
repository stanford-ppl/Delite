package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Mat[@specialized(Boolean, Int, Long, Float, Double) T] extends DeliteCollection[T] with Traversable[T] {
  def apply(n: Int, m: Int) : T
  def update(n: Int, m: Int, v : T) : Unit
  def size : Int
  
  def apply(idx: Int) : T
  def update(idx: Int, v: T) : Unit

  def row(n: Int) : MatRow[T]
  def col(n: Int) : MatCol[T]

  def numRows : Int
  def numCols : Int

    // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit
  def dcSize : Int = size
  
  def cloneL : Mat[T]
  
  def foreach[U](f: T => U) = for(i <- 0 until size) f(this(i))
}

trait VecView[@specialized(Boolean, Int, Long, Float, Double) T] extends Vec[T]

trait MatRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VecView[T]

trait MatCol[@specialized(Boolean, Int, Long, Float, Double) T] extends VecView[T]

object Mat {
  def ofSize[T: Manifest](rows: Int, cols: Int) = {
    new MatImpl[T](rows, cols)
  }
  
  def apply[T: Manifest](xs: Vec[T]*) = {
    new MatImpl[T](xs.size, xs(0).size, xs.flatten.toArray)
  }
}
