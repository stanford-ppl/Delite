package ppl.dsl.optiql.baseline.tests.simple

import ppl.dsl.optiql.OptiQL

object SimpleTest {
  def main(args: Array[String]) {
    import OptiQL._

    val calls = CallLog.SampleData()
    val contacts = Contact.SampleData()


    val res = calls.Join(contacts)(_.Number, _.Phone, (call, contact) => new {
      val Name = contact.FirstName + " " + contact.LastName
      val Number = call.Number
      val Duration = call.Duration
    })

//    val res2 = res GroupBy(_.Number) Select(g => new {
//        val Number = g.key
//        val TotalDuration = g.Sum(_.Duration)
//        val AverageDuration = g.Average(_.Duration)
//        val NumberOfCalls = g.Count
//      });

    res.printAsTable
  }
}