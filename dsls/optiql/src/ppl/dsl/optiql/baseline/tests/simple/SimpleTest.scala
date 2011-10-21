package ppl.dsl.optiql.baseline.tests.simple

import ppl.dsl.optiql.baseline.OptiQL
import ppl.dsl.optiql.baseline.util.Date

object SimpleTest {
  def main(args: Array[String]) {
    import OptiQL._

    val calls = CallLog.SampleData()
    val contacts = Contact.SampleData()

    val q4 = calls.Join(contacts)(_.Number, _.Phone, (call, contact) => new {
      val Name = contact.FirstName + " " + contact.LastName
      val Number = call.Number
      val Duration = call.Duration
    }) GroupBy(_.Name) Select(g => new {
      val Name = g.key
      val TotalDuration = g.Sum(_.Duration)
      val AverageDuration = g.Average(_.Duration)
    }) OrderBy (_.Name)
    q4.printAsTable



  }
}