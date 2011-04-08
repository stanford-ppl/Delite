package ppl.dsl.optiql.baseline.tests.simple

import ppl.dsl.optiql.OptiQL

object SimpleTest {
  def main(args: Array[String]) {
    import OptiQL._

    val res = CallLog.SampleData  Where(_.Duration < 30) OrderBy(_.Incoming) ThenBy(_.Duration)

//    val res2 = res GroupBy(_.Number) Select(g => new {
//        val Number = g.key
//        val TotalDuration = g.Sum(_.Duration)
//        val AverageDuration = g.Average(_.Duration)
//        val NumberOfCalls = g.Count
//      });

    res.printAsTable
  }
}