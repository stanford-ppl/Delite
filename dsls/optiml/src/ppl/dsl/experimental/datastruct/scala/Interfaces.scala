package ppl.dsl.experimental.datastruct.scala

// this file should not be needed using pure static dispatch!

/**
 * Vector
 */

/*
trait Vector[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  def length: Int
  def isRow: Boolean
  def mtrans: Vector[T]  
  def apply(n: Int): T
  def update(index: Int, x: T)
  def cloneL: Vector[T]
  def toList: List[T]
  def sort(implicit o: Ordering[T]): Vector[T] // because we use the underlying data field to sort
  def copyFrom(pos: Int, xs: Vector[T])
  def insert(pos: Int, x: T)
  def insertAll(pos: Int, xs: Vector[T])
  def removeAll(pos: Int, len: Int)
  def clear()
  def trim
    
  // DeliteCollection
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def dcSize = length
  
  // value equality is needed for DeliteCollection zero
  override def equals(rhs: Any): Boolean = {
    if (!rhs.isInstanceOf[Vector[T]]) return false
        
    val rv = rhs.asInstanceOf[Vector[T]]
    if (dcSize != rv.dcSize) return false
    var i = 0
    while (i < rv.dcSize) {
      if (dcApply(i) != rv.dcApply(i)) return false
      i += 1        
    }
    true    
  }
  
  // TO BE MOVED TO DenseVector
  def data: Array[T]
  def unsafeSetData(xs: Array[T], len: Int)
}

// TODO: enable this after switching VectorOps to DenseVectorOps
trait DenseVector[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T] {
//   def data: Array[T]
//   def unsafeSetData(xs: Array[T], len: Int)  
}

trait SparseVector[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T]
*/