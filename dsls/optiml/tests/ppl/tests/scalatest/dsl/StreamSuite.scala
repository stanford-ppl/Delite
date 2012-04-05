package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication, OptiMLExp}
import ppl.dsl.optiml.{Vector, Stream}
import ppl.tests.scalatest._

/* Testing stream operations
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 3/14/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object StreamInitSmallRunner extends DeliteTestRunner with OptiMLApplicationRunner with StreamInitSmall
trait StreamInitSmall extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val s = Stream[Int](1000, 1000){ (i,j) => random[Int] }

    // just testing for no initialization errors
    collect(true)

    mkReport
  }
}

object StreamInitPureLargeRunner extends DeliteTestRunner with OptiMLApplicationRunner with StreamInitPureLarge
trait StreamInitPureLarge extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val s = Stream[Int](1000000, 10000){ (i,j) => i+j }

    // just testing for no initialization errors
    collect(s.isPure)

    mkReport
  }
}

object StreamInitEffectLargeRunner extends DeliteTestRunner with OptiMLApplicationRunner with StreamInitEffectLarge
trait StreamInitEffectLarge extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val s = Stream[Int](1000000, 10000){ (i,j) => random[Int] }

    // just testing for no initialization errors
    collect(!s.isPure)

    mkReport
  }
}

object StreamForeachRunner extends DeliteTestRunner with OptiMLApplicationRunner with StreamForeach
trait StreamForeach extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val s = Stream[Int](100000, 1000){ (i,j) => random[Int] }
    s.foreachRow { v => collect(v.length == 1000 && v.sum.abs > 0) }

    mkReport
  }
}

object StreamCorrectSmallRunner extends DeliteTestRunner with OptiMLApplicationRunner with StreamCorrectSmall
trait StreamCorrectSmall extends DeliteTestModule with OptiMLApplication {
  def main() = {

    //should be row vectors starting at 0, 10, ... 90
    val s = Stream[Double](10,10){ (i,j) => i*10+j }
    s.foreachRow { v => collect(v(2) == v.index*10+2) }
    
    mkReport
  }
}

object StreamCorrectLargeRunner extends DeliteTestRunner with OptiMLApplicationRunner with StreamCorrectLarge
trait StreamCorrectLarge extends DeliteTestModule with OptiMLApplication {
  def main() = {

    //should be row vectors starting at 0, 10, ... 109990
    val s = Stream[Double](11000,10){ (i,j) => i*10+j }
    s.foreachRow { v => collect(v(9) == v.index*10+9) }
    //s.foreachRow { _.pprint }

    mkReport
  }
}

class StreamSuite extends DeliteSuite {
  def testStreamInitSmall() { compileAndTest(StreamInitSmallRunner) }
  def testStreamInitPureLarge() { compileAndTest(StreamInitPureLargeRunner) }
  def testStreamInitEffectLarge() { compileAndTest(StreamInitEffectLargeRunner) }
  def testStreamForeach() { compileAndTest(StreamForeachRunner) }
  def testStreamCorrectSmall() { compileAndTest(StreamCorrectSmallRunner) }
  def testStreamCorrectLarge() { compileAndTest(StreamCorrectLargeRunner) }
  // def testFileStream() ...
}