package ppl.tests.scalatest.delite

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._
import scala.virtualization.lms.common.Record

/*
 * Tests Delite correctness in the presence of mutable writes, including nested writes that require IR rewrites.
 */

object SimpleWritesRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with SimpleWrites
trait SimpleWrites extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {
    val a = DeliteArray[Double](10)

    a(5) = 10
    a(3) = 2.7
    a(7) = a(3) + a(5)
    a(9) = a(5)

    collect(a(5) == 10)
    collect(a(3) == 2.7)
    collect(a(7) == 12.7)
    collect(a(9) == 10)

    a(9) = 0.01
    a(3) = 31.2
    a(4) = 9.6

    collect(a(9) == 0.01)
    collect(a(3) == 31.2)
    collect(a(4) == 9.6)

    mkReport
  }
}

// it is possible that aliasSyms and friends do not handle this case because it is nested mutable, which is supposed to be disallowed
// although, it appears that this case would break even if the outer array was immutable (immutable collection of mutable objects), so maybe there is just a bug w/extractSyms

// by rewriting nested updates to DeliteArray and only reflecting on the outer array, we have actually broken the model, since nothing
// prevents us from binding to an inner element (and now we no longer get an error alerting us to the problem)

object NestedWritesRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with NestedWrites
trait NestedWrites extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {
    val b = (DeliteArrayBuffer.fromFunction(10) { i => DeliteArray.fromFunction(20) { j => 1.0 } }).mutable

    val a = b(5)
    a(5) = 9.2
    a(13) = 1.9

    collect(a(5) == 9.2)
    collect(a(13) == 1.9)

    b(2).update(7,5.6)
    b(5).update(5,3.2)
    b(8).update(1,8.2)

    b(7) = DeliteArray.fromFunction(10) { i => 0.0 }
    b(8) = DeliteArray.fromFunction(10) { i => 0.0 }

    collect(a(5) == 3.2) // FAILS - update to b is not seen by a, even when aliasSyms and/or extractSyms are defined for DeliteArrayApply
    collect(b(5).apply(5) == 3.2)
    collect(a(13) == 1.9)
    collect(b(5).apply(13) == 1.9)

    val a2 = b(8)

    collect(b(8).apply(1) == 0.0)
    collect(a2(1) == 0.0)

    a2(6) = 5
    collect(a2(6) == 5)
    collect(b(8).apply(6) == 5)

    mkReport
  }
}

class MutabilitySuite extends DeliteSuite {
  // def testSimpleWrites() { compileAndTest(SimpleWritesRunner) }
  def testNestedWrites() { compileAndTest(NestedWritesRunner) }
}

