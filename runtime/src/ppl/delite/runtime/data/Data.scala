package ppl.delite.runtime.data

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:52:32 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class Data[T] {

  def size : Int

  def apply(i: Int) : T

  def update(i: Int, x: T) : Unit

}