package generated.scala


/**
 * Delite
 */

abstract class DeliteOpMultiLoop[A] {
  def size: Long
  var loopStart: Long
  var loopSize: Long
  def alloc: A
  def processRange(__act: A, start: Long, end: Long, tid: Long): A //init+process
  def combine(__act: A, rhs: A, tid: Long): Unit
  def postCombine(__act: A, rhs: A, tid: Long): Unit
  def postProcInit(__act: A, tid: Long): Unit
  def postProcess(__act: A, tid: Long): Unit
  def finalize(__act: A, tid: Long): Unit
  def initAct: A
}

/**
 * Ref
 */

class Ref[@specialized T](v: T) {
  private[this] var _v = v

  def get = _v
  def set(v: T) = _v = v
}
