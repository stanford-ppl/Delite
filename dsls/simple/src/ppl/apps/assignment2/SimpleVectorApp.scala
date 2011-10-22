package ppl.apps.assignment2

import ppl.dsl.assignment2.{SimpleVectorApplicationRunner, SimpleVectorApplication}

object SimpleVectorAppRunner extends SimpleVectorApplicationRunner with SimpleVectorApp

trait SimpleVectorApp extends SimpleVectorApplication {

  def main() {
    val x = Vector[Int](100) + 1
    val y = Vector[Int](100) + 2

    val z = x + y
    z.pprint

    //val f = z.filter
    //f.pprint
    
    //val res = z.sum
    //println(res)
  }
}
