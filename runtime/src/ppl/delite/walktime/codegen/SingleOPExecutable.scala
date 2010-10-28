package ppl.delite.walktime.codegen

import java.util.concurrent.locks.ReentrantLock
import collection.mutable.ArrayBuffer

/**
 * Author: Kevin J. Brown
 * Date: Oct 27, 2010
 * Time: 9:09:56 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * the fast and dirty way to execute tasks
 * the good: functionally correct
 * the issues:
 *  1) all kernel inputs have to be passed as a collection
 *  2) the kernel itself has to be passed in as a function
 *  3) relating dependencies to inputs is tricky/fragile
 *  4) good static typing is non-existent
 *
 */

class SingleOPExecutable[T](kernel: Seq[Any] => T) extends DeliteExecutable {

  var _result: T = _
  var notReady: Boolean = true

  val lock = new ReentrantLock
  val condition = lock.newCondition

  val dependencies = new ArrayBuffer[SingleOPExecutable[_]]

  def addDependency(dep: SingleOPExecutable[_]) {
    dependencies += dep  
  }

  def run() {
    val inputs = new ArrayBuffer[Any](dependencies.length)
    //get dependencies
    for (dep <- dependencies) {
      inputs += dep.get
    }
    //TODO: this dependency passing scheme is painfully fragile and doesn't account for anti-dependencies
    //execute
    val x = kernel(inputs)

    //set result
    set(x)

  }

  def get: T = {
    if (notReady) block
    _result
  }

  private def block {
    lock.lock
    try {
      while (notReady) condition.await
    }
    finally {
      lock.unlock
    }
  }

  private def set(result: T) {
    lock.lock
    try {
      _result = result
      notReady = false
      condition.signalAll
    }
    finally {
      lock.unlock
    }
  }

}