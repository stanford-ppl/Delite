package generated.scala


/**
 * Delite
 */

abstract class DeliteOpMultiLoop[A] {
  def size(info: ResourceInfo): Long
  var loopStart: Long
  var loopSize: Long
  def alloc(info: ResourceInfo): A
  def processRange(info: ResourceInfo, __act: A, start: Long, end: Long): A //init+process
  def combine(info: ResourceInfo, __act: A, rhs: A): Unit
  def postCombine(info: ResourceInfo, __act: A, rhs: A): Unit
  def postProcInit(info: ResourceInfo, __act: A): Unit
  def postProcess(info: ResourceInfo, __act: A): Unit
  def finalize(info: ResourceInfo, __act: A): Unit
  def initAct(info: ResourceInfo): A
}

/**
 * Ref
 */

final class Ref[@specialized T](v: T) {
  private[this] var _v = v

  final def get = _v
  final def set(v: T) = _v = v
}
