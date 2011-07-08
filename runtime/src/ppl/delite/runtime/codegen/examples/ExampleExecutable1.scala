package ppl.delite.runtime.codegen.examples

import java.util.concurrent.locks.ReentrantLock
import ppl.delite.runtime.data.Data
import ppl.delite.runtime.codegen.DeliteExecutable

/**
 * Author: Kevin J. Brown
 * Date: Oct 25, 2010
 * Time: 11:49:22 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This is an example of what a generated DeliteExecutable should look like; it should not be invoked
 * Ex0 shows how to consume results from other threads
 * Ex1 shows how to produce results for other threads
 */

object ExampleExecutable1 extends DeliteExecutable {

  val thread0 = ExampleExecutable0

  def run() {

    val x1 = kernel1_run(0)
    Res1.set(x1)

    val x2 = kernel2_run(x1)
    Res2.set(x2)

  }

  def get1: Data[Int] = Res1.get

  def get2: Int = Res2.get

  //TODO: we can encapsulate each of these in an object or list them all out in the outer object
  //TODO: what is the better solution? (performance, gc, etc.)

  private object Res1 {

    private var notReady: Boolean = true
    private var res: Data[Int] = _

    private val lock = new ReentrantLock
    private val cond = lock.newCondition

    /**
     * getter: used to pass result to other threads
     * implementation: a blocking read
     */

    def get: Data[Int] = {
      if (notReady) block //try to avoid locking
      res
    }

    private def block {
      lock.lock
      try {
        while (notReady) cond.await
      }
      finally {
        lock.unlock
      }
    }

    /**
     * setters: used to set results as complete so they can be passed to other threads
     * implementation: store result and signal ready
     */
    def set(result: Data[Int]) {
      lock.lock
      try {
        res = result
        notReady = false
        cond.signalAll //signalling no one should be cheap (null check)
      }
      finally {
        lock.unlock
      }
    }

  }

  private object Res2 {

    private var notReady: Boolean = true
    private var res: Int = _

    private val lock = new ReentrantLock
    private val cond = lock.newCondition

    def get: Int = {
      if (notReady) block
      res
    }

    private def block {
      lock.lock
      try {
        while (notReady) cond.await
      }
      finally {
        lock.unlock
      }
    }

    def set(result: Int) {
      lock.lock
      try {
        res = result
        notReady = false
        cond.signalAll
      }
      finally {
        lock.unlock
      }
    }

  }

  //ignore everything below
  def kernel1_run(arg0: Int): Data[Int] = null

  def kernel2_run(arg0: Data[Int]): Int = 0

}