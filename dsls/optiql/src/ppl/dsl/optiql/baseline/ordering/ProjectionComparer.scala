package ppl.dsl.optiql.baseline.ordering

class ProjectionComparer[TElement, TKey](keySelector: TElement => TKey)(implicit comparer: Ordering[TKey]) extends Ordering[TElement] {
   def compare(x: TElement, y: TElement) = {
    val keyX = keySelector(x)
    val keyY = keySelector(y)
    comparer.compare(keyX, keyY)
  }
}