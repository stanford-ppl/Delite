package ppl.tests.scalatest.dsl.optiql

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import ppl.tests.scalatest._


trait TestRecord extends OptiQLApplication {

  type Item = Record {
    val id: Int
    val quantity: Int
    val price: Double
    val status: Char
  }

  def Item(_id: Rep[Int], _quantity: Rep[Int], _price: Rep[Double], _status: Rep[Char]) = new Record {
    val id = _id
    val quantity = _quantity
    val price = _price
    val status = _status
  }

  val items = Table(Item(0, 10, 2.49, 'N'), Item(1, 0, 49.95, 'B'), Item(2, 1000, 0.99, 'N'), Item(3, 18, 5.99, 'S'))
  val itemsSize = 4

  val emptyTable = Table[Item]()

}

object QueryableSelectRunner extends DeliteTestRunner with OptiQLApplicationRunner with QueryableSelectTest
trait QueryableSelectTest extends DeliteTestModule with OptiQLApplication with TestRecord {
  def main() {
    val scalarResult = items Select _.id
    
    collect(resut.size == itemsSize)
    for (i <- 0 until itemsSize) {
      collect(result(i) == i)
    }

    val result = items Select(item => new Result {
      val id = item.id
      val maxRevenue = item.quantity * item.price
    })

    collect(result.size == itemsSize)

    for (i <- 0 until itemsSize) {
      collect(result(i).id == i)
    }

    collect(result(0).maxRevenue == 24.9)
    collect(result(2).maxRevenue == 0)
    collect(result(1).maxRevenue == 990)
    collect(result(3).maxRevenue == 107.82)
    mkReport
  }
}

object QueryableWhereRunner extends DeliteTestRunner with OptiQLApplicationRunner with QueryableWhereTest
trait QueryableWhereTest extends DeliteTestModule with OptiQLApplication with TestRecord {
  def main() {
    val result = items Where(_.status == 'N') Select(item => new Result {
      val id = item.id
      val maxRevenue = item.quantity * item.price
    })

    collect(result.size == 2)

    collect(result.First.id == 0)
    collect(result.Last.id == 2)

    collect(result.First.maxRevenue == 24.9)
    collect(result.Last.maxRevenue == 990)

    val res2 = items Where(_.status == 'T') Select(item => id)
    collect(res2.size == 0)
    mkReport
  }
}

object QueryableReduceRunner extends DeliteTestRunner with OptiQLApplicationRunner with QueryableReduceTest
trait QueryableReduceTest extends DeliteTestModule with OptiQLApplication with TestRecord {
  def main() {
    val sumQuantity = items Sum(_.quantity)
    collect(sumQuantity == 1028)

    val minQuantity = items Min(_.quantity)
    collect(minQuantity == 0)

    val maxId = items Max(_.id)
    collect(maxId == 3)

    val avgPrice = items Average(_.price)
    collect(avgPrice == 14.855)
    mkReport
  }
}

object QueryableGroupByRunner extends DeliteTestRunner with OptiQLApplicationRunner with QueryableGroupByTest
trait QueryableGroupByTest extends DeliteTestModule with OptiQLApplication with TestRecord {
  def main() {
    val res1 = items GroupBy(_.status) Select(g => new Result {
      val status = g.key
      val sumQuantity = g.Sum(_.quantity)
      val count = g.Count
    })
    collect(res1.size == 3)
    collect(res1(0).status == 'N' && res1(0).sumQuantity == 1010 && res1(0).count == 2)
    collect(res1(1).status == 'B' && res1(1).sumQuantity == 0 && res1(1).count == 1)
    collect(res1(2).status == 'S' && res1(2).sumQuantity == 18 && res1(2).count == 1)

    val res2 = items Where(_.quantity > 0) GroupBy(_.status) Select(g => new Result {
      val status = g.key
      val sumQuantity = g.Sum(_.quantity)
      val maxQuantity = g.Max(_.quantity)
      val avgPrice = g.Average(_.price)
      val count = g.Count
    })
    collect(res2.size == 2)
    collect(res2.First.status == 'N' && res2.First.sumQuantity == 1010 && res2.First.maxQuantity == 1000 && res2.First.avgPrice == 1.74 && res2.First.count == 2)
    collect(res2.Last.status == 'S' && res2.Last.sumQuantity == 18 && res2.Last.maxQuantity == 18 && res2.Last.avgPrice == 5.99 && res2.Last.count == 1)
  
    val res3 = items Where(_.status == 'T') GroupBy(_.status) Select(g => g.key)
    collect(res3.size == 0)

    val res4 = emptyTable GroupBy (_.status) Select(g => g.key)
    collect(res4.size == 0)
    mkReport
  }
}

object QueryableSortRunner extends DeliteTestRunner with OptiQLApplicationRunner with QueryableSortTest
trait QueryableSortTest extends DeliteTestModule with OptiQLApplication with TestRecord {
  def main() {
    val sort1 = items OrderBy(_.id)
    for (i <- 0 until itemsSize) {
      collect(sort1(i).id == i)
    }

    val sort2 = items OrderBy(_.quantity) ThenByDescending(_.price)
    collect(sort2(0).quantity == 0)
    collect(sort2(1).quantity == 10)
    collect(sort2(2).quantity == 18)
    collect(sort2(3).quantity == 1000)

    val sort3 = items OrderByDescending(_.status) ThenBy(_.quantity)
    collect(sort3(0).status == 'S')
    collect(sort3(1).status == 'N' && sort3(1).quantity == 10)
    collect(sort3(2).status == 'N' && sort3(2).quantity == 1000)
    collect(sort3(3).status == 'B')
    mkReport
  }
}

/*object QueryableJoinRunner extends DeliteTestRunner with OptiQLApplicationRunner with QueryableJoinTest
trait QueryableJoinTest extends DeliteTestModule with OptiQLApplication with TestRecord {
  def main() {
    //TODO
  }
}*/


class QuerySuite extends DeliteSuite {
  def testSelect() { compileAndTest(QueryableSelectRunner) }
  def testWhere() { compileAndTest(QueryableWhereRunner) }
  def testReduce() { compileAndTest(QueryableReduceRunner) }
  def testGroupBy() { compileAndTest(QueryableGroupByRunner) }
  def testSort() { compileAndTest(QueryableSortRunner) }
}

