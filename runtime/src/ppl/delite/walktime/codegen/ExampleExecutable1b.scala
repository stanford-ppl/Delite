package ppl.delite.walktime.codegen

import ppl.delite.data.Data
import java.util.concurrent.locks.ReentrantLock

/**
 * Author: Kevin J. Brown
 * Date: Oct 26, 2010
 * Time: 3:47:24 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class ExampleExecutable1b extends DeliteExecutable {


  def run() {

    val x1 = kernel1_run(0)
    set1(x1)

    val x2 = kernel2_run(x1)
    set2(x2)

  }  

  //TODO: we can encapsulate each of these in an object or list them all out in the outer object
  //TODO: what is the better solution? (performance, gc, etc.)

  private val lock1 = new ReentrantLock
  private val cond1 = lock1.newCondition
  private var notReady1: Boolean = true
  private var res1: Data[Int] = _

  private val lock2 = new ReentrantLock
  private val cond2 = lock2.newCondition
  private var notReady2: Boolean = true
  private var res2: Int = _

  def get1: Data[Int] = {
    if (notReady1) block1
    res1
  }

  def get2: Int = {
    if (notReady2) block2
    res2
  }

  private def block1 {
    lock1.lock
    try {
      while (notReady1) cond1.await
    }
    finally {
      lock1.unlock
    }
  }

  private def block2 {
    lock2.lock
    try {
      while (notReady2) cond2.await
    }
    finally {
      lock2.unlock
    }
  }

  private def set1(res: Data[Int]) {
    lock1.lock
    try {
      res1 = res
      notReady1 = false
      cond1.signalAll
    }
    finally {
      lock1.unlock
    }
  }

  private def set2(res: Int) {
    lock2.lock
    try {
      res2 = res
      notReady2 = false
      cond2.signalAll
    }
    finally {
      lock2.unlock
    }
  }

  //ignore everything below
  def kernel1_run(arg0: Int): Data[Int] = null

  def kernel2_run(arg0: Data[Int]): Int = 0

}