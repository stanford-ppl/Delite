package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object TupleTestRunner extends OptiMLApplicationRunner with TupleTest

trait TupleTest extends OptiMLApplication {

  def main() = {

    val vec0 = Vector.onesf(10)
    val t = make_tuple2(unit(3),vec0(1))
    val vec1 = Vector.onesf(100)
    val vec2 = vec1.map(ele =>
      {
        val dd = make_tuple2(ele,ele)
        dd._1 * ele * t._2
      }
    )
    vec2.pprint
  }
}
