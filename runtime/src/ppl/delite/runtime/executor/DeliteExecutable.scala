package ppl.delite.runtime.executor

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 2:12:33 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteExecutable {

  def run() : Unit

  def self = this

}
