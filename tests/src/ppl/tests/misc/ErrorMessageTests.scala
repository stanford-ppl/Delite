package ppl.tests.misc

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object ErrorMessageTestsRunner extends OptiMLApplicationRunner with ErrorMessageTests

trait ErrorMessageTests extends OptiMLApplication {
  class Point // just an example, should be a real type the user thinks they can use (either because they defined it or
              // they found it in another library)
 
  def main() = {
    // create two length 3 Vectors of Point objects
    val v1 = Vector[Point](3,true)
    val v2 = Vector[Point](3,true)

    // ex. 1 try to add these vectors: produces a static error because Point is not a built-in DSL type and it does not
    // support the Numeric interface
//    v1 + v2

    // produces a Scala error message that makes sense to a Scala user, but probably not a DSL user:
    // error: could not find implicit value for parameter n: Numeric[ppl.tests.ErrorMessageTests.Point]
    // v1 + v2



    // ex. 2 embedding implementation details exposed to user (Rep[T])
    //error: type mismatch;
    //found   : ppl.tests.ErrorMessageTests.Rep[Double]
    //required: ppl.tests.ErrorMessageTests.Rep[Int]
    //v3(5) = v4(5)

//    val v3 = Vector[Int](100)
//    val v4 = Vector[Double](100)
//    v3(5) = v4(5)


    // ex. 3 for demonstration purposes: undefined method results in unrelated Scala error message due to
    // Scala default semantics (toString)
    //error: type mismatch;
    //found   : ppl.tests.ErrorMessageTests.Point
    //required: String
    //p1 + p2

//    val p1 = new Point
//    val p2 = new Point
//    p1 + p2


    // much worse errors likely to occur in the future due to code generation. how do we make sense of (and report)
    // a CUDA error message to an application developer writing in Scala?
  }
 
}