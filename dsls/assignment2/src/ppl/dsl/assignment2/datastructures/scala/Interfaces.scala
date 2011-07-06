package generated.scala

/**
 * SimpleVector
 */

trait Vector[@specialized(Int, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  def length: Int
  def apply(idx: Int): T
  def update(idx: Int, value: T)

  //Delite Collection interface
  def size = length
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, value: T) = update(idx, value)
}


