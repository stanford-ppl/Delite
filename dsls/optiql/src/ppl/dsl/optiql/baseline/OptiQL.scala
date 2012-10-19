package ppl.dsl.optiql.baseline

import containers.{Grouping, Table}
import ordering.{ProjectionComparer, OrderedQueryable}
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import collection.mutable.Buffer

object OptiQL {

  implicit def convertIterableToQueryable[T](i: Iterable[T]) = new Queryable[T](i)
  implicit def convertIterableToTable[T](i: Iterable[T]) : Table[T] = {
    if(i.isInstanceOf[Table[T]]) {
      i.asInstanceOf[Table[T]]
    }
    else if(i.isInstanceOf[ArrayBuffer[T]]) {

      return new Table[T] {

        override val data = i.asInstanceOf[ArrayBuffer[T]]

        def addRecord(arr: Array[String]) {
          throw new RuntimeException("Cannot add Record into a projected Table")
        }
      }

    }
    else {
      val arrbuf = new ArrayBuffer[T]();
      for (e <- i) {
        arrbuf.append(e)
      }
      return new Table[T] {

        override val data = arrbuf

        def addRecord(arr: Array[String]) {
          throw new RuntimeException("Cannot add Record into a projected Table")
        }
      }
      //throw new RuntimeException("Could not convert iterable to Table")
    }
  }


}

class Queryable[TSource](source: Iterable[TSource]) {
  import OptiQL._

  def Where(predicate: TSource => Boolean) =  {
    if(predicate == null) throw new IllegalArgumentException("Predicate is Null")
    source.filter(predicate)
  }

  def Where(predicate: (TSource, Int) => Boolean) = {
    if(predicate == null) throw new IllegalArgumentException("Predicate is Null")
    val res = new ArrayBuffer[TSource]
    res.sizeHint(source.size/2)
    var i = 0
    for(element <- source) {
      if(predicate(element,i))
        res.append(element)
      i += 1
    }
    res
  }

  def Select[TResult](selector: TSource => TResult) = {
    source.map(selector)
  }

  def OrderBy[TKey](keySelector: TSource => TKey)(implicit comparer: Ordering[TKey]) = {
    new OrderedQueryable(source, new ProjectionComparer(keySelector))
  }



  def GroupBy[TKey](keySelector: TSource => TKey): Iterable[Grouping[TKey, TSource]] = {
    val (hTable, keys) = buildHash(source,keySelector)
    val result = new Table[Grouping[TKey,TSource]] {
      def addRecord(fields: Array[String]) = throw new RuntimeException("Cannot add records to a grouping table")
      override val grouped = true
    }
    for(key <- keys) {
      result.data += new Grouping(key,hTable.getOrElse(key, new ArrayBuffer[TSource]))
    }
    result
  }

  //TODO: Slow implementation
  def Join[TInner, TKey, TResult](inner: Iterable[TInner])(sourceKeySelector: TSource => TKey,
                                  innerKeySelector: TInner => TKey,
                                  resultSelector: (TSource,TInner) => TResult) = {
    for(sourceElement <- source; innerElement <- inner;
        if sourceKeySelector(sourceElement) == innerKeySelector(innerElement))
      yield resultSelector(sourceElement,innerElement)

  }

  def Count = source.size

  def Sum[@specialized T:Numeric](selector: TSource => T): T = {
    val n = implicitly[Numeric[T]]
    import n._
    var sum = n.zero
    for(e <- source) {
      sum += selector(e)
    }
    sum
  }


  def Average[@specialized T:Numeric](selector: TSource => T): Float = {
    val n = implicitly[Numeric[T]]
    import n._
    Sum(selector).toFloat/Count
  }


  /*****
   * Internal Implementation functions
   */
  private def buildHash[TKey](source:Iterable[TSource], keySelector: TSource => TKey) = {
    val hash = HashMap[TKey, Buffer[TSource]]()
    val keys = new ArrayBuffer[TKey]
    for (elem <- source; key = keySelector(elem)) {
      hash.getOrElseUpdate(key,{
        keys.append(key)
        new ArrayBuffer[TSource]() //if there is no key
      }) += elem
    }
    (hash,keys)
  }

  private def ni = throw new RuntimeException("Not Implemented")

}