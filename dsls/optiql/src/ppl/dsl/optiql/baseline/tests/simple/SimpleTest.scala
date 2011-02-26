package ppl.dsl.optiql.baseline.tests.simple

import ppl.dsl.optiql.OptiQL

object SimpleTest {
  def main(args: Array[String]) {
    import OptiQL._

    val res = Contact.SampleData().Where(c => c.State == "WA")

    println("Result: " + res)
  }
}