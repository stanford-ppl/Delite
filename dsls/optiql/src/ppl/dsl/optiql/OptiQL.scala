package ppl.dsl.optiql;

object OptiQL {

  implicit def convertIterableToQueryable[T](i: Iterable[T]) = new Queryable[T](i)

}

class Queryable[T](q: Iterable[T]) {

  def Where(p: T => Boolean) =  {
    q.filter(p)
  }

}