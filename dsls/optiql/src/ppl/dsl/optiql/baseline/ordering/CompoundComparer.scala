package ppl.dsl.optiql.baseline.ordering

class CompoundComparer[T](primary: Ordering[T], secondary: Ordering[T]) extends Ordering[T] {
  def compare(x: T, y: T): Int = {
    val primaryResult = primary.compare(x,y)
    if(primaryResult != 0)
      return primaryResult
    return secondary.compare(x,y)
  }
}