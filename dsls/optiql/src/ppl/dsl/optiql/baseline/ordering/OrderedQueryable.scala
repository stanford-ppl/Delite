package ppl.dsl.optiql.baseline.ordering

import scala.util.Sorting
import collection.mutable.ArrayBuffer

class OrderedQueryable[TElement](source: Iterable[TElement], currentComparer: Ordering[TElement]) extends Iterable[TElement] {

  def createOrderedQueryable[TKey](keySelector: TElement => TKey, comparer: Ordering[TKey], descending: Boolean = false): OrderedQueryable[TElement] = {
    if(keySelector == null) throw new IllegalArgumentException("keySelector is null")
    val secondaryComparer = new ProjectionComparer(keySelector)(comparer)
    return new OrderedQueryable(source,new CompoundComparer(currentComparer, secondaryComparer))
  }

  //TODO, should be able to create an Array using manifests, once the manifest bug is fixed.
  def iterator = {
    val toBeSorted  = source.asInstanceOf[Seq[TElement]]
    val sorted = toBeSorted.sorted(currentComparer)
    //Sorting.quickSort(toBeSorted)(currentComparer)
    sorted.iterator
  }

  def ThenBy[TKey](keySelector: TElement => TKey)(implicit comparer: Ordering[TKey]) = {
    createOrderedQueryable(keySelector, comparer)
  }
}