package ppl.delite.runtime.messages

import java.util.concurrent.locks.ReentrantLock

class Future[T] {

  private var notReady: Boolean = true
  private var _result: T = _

  private val lock = new ReentrantLock
  private val cond = lock.newCondition

  def get: T = {
    if (notReady) block
    _result
  }

  private def block {
    val lock = this.lock
    lock.lock
    try {
      while (notReady)
        cond.await
    }
    finally {
      lock.unlock
    }
  }

  def set(result: T) {
    val lock = this.lock
    lock.lock
    try {
      _result = result
      notReady = false
      cond.signalAll
    }
    finally {
      lock.unlock
    }
  }

}
