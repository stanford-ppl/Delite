package ppl.tests.scalatest.delite

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._

object DeliteGPUCondRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUCond
trait DeliteGPUCond extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val v1 = DeliteArrayBuffer.fromFunction(10)(i => 1.0f)
    val out = if (v1(0) == 1.0f) {
      val v2 = v1.zip(DeliteArrayBuffer.fromFunction(10)(i =>1.0f)){ _ + _ }
      v2(0)
    } else {
      v1(0)
    }

    collect(out == 2.0f)

    mkReport
  }
}

object DeliteGPUCondReturnRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUCondReturn
trait DeliteGPUCondReturn extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val v1 = DeliteArrayBuffer.fromFunction(10)(i => 1.0f).mutable
    v1(3) = 3.4f
    val v2 = DeliteArrayBuffer.fromFunction(10)(i => 0.0f).mutable
    v2(5) = 2.8f

    val out = if (v1(0) != 1.0f) v1.zip(v1){ _ + _ }
              else v1.zip(v2){ _ - _ }

    collect(out(0) == 1.0f)
    collect(out(3) == 3.4f)
    collect(out(5) == -1.8f)

    mkReport
  }
}

object DeliteGPUMemLeakRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUMemLeak
trait DeliteGPUMemLeak extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    var i = 0
    while (i < 1000) {
      val a1 = DeliteArrayBuffer.fromFunction(4000*4000)(i => 1.0f)
      val out = a1.map(e => e + 1.0f)
      collect(out(0) == 2.0f)
      i += 1
    }

    mkReport
  }
}

object DeliteGPUMutationRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUMutation
trait DeliteGPUMutation extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val v1 = DeliteArrayBuffer.fromFunction(10)(i => 0.0f).mutable
    v1(4) = 3.4f
    val out = v1.filter(e => e > 2.0f)

    collect(out.length == 1)
    collect(out(0) == 3.4f)

    mkReport
  }
}

object DeliteGPUNestedMutationRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUNestedMutation
trait DeliteGPUNestedMutation extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    var i = 0
    while (i < 2) {
      val out = DeliteArrayBuffer.fromFunction(10*10)(i => 1.0f).mutable
      val temp = DeliteArrayBuffer.fromFunction(10*10)(i => 1.0f)
      out forIndices { i => out(i) = out(i) + temp(i) }
      collect(out(0) == 2.0f)
      i += 1
    }

    mkReport
  }
}

object DeliteGPUObjectReductionRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUObjectReduction
trait DeliteGPUObjectReduction extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {
    val rows = 4
    val cols = 4096

    val in = DeliteArrayBuffer.fromFunction(rows*cols)(i => 1.0f)
    val out = DeliteArrayBuffer.fromFunction(rows) { i => 
      DeliteArrayBuffer.fromFunction(cols)(j => in(i*cols+j)).reduce(_ + _)(0.0f)
    }

    collect(out(0) == 4096.0f)
    collect(out(1) == 4096.0f)
    collect(out(2) == 4096.0f)
    collect(out(3) == 4096.0f)
    
    mkReport
  }
}

object DeliteGPUReferencePrimitive1Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUReferencePrimitive1
trait DeliteGPUReferencePrimitive1 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val v = DeliteArrayBuffer.fromFunction(10)(i => 1).mutable

    val result = if(v(0) < 2) {
      v forIndices { i => v(i) = i }
      v.reduce(_ + _)(0) // returned by GPU (returner), and is referential primitive
    }
    else {
      3
    }

    collect(result == 45)

    mkReport
  }
}

object DeliteGPUReferencePrimitive2Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUReferencePrimitive2
trait DeliteGPUReferencePrimitive2 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val v = DeliteArrayBuffer.fromFunction(10)(i => 1).mutable
    val s = v.reduce(_ + _)(0) // s is referential primitive, passed into conditional block as input
    collect(s == 10)

    val result = if(v(0) < 2) {
      v forIndices { i => v(i) = s }
      v(0)
    }
    else {
      3
    }

    collect(result == 10)

    mkReport
  }
}

class DeliteGPUSuite extends DeliteSuite {
  def testDeliteGPUCond() { compileAndTest(DeliteGPUCondRunner, CHECK_MULTILOOP); }
  def testDeliteGPUCondReturn() { compileAndTest(DeliteGPUCondReturnRunner, CHECK_MULTILOOP); }
  def testDeliteGPUMemLeak() { compileAndTest(DeliteGPUMemLeakRunner, CHECK_MULTILOOP); }
  def testDeliteGPUMutation() { compileAndTest(DeliteGPUMutationRunner, CHECK_MULTILOOP); }
  def testDeliteGPUNestedMutation() { compileAndTest(DeliteGPUNestedMutationRunner, CHECK_MULTILOOP); }
  def testDeliteGPUObjectReduction() { compileAndTest(DeliteGPUObjectReductionRunner, CHECK_MULTILOOP); }
  def testDeliteGPUReferencePrimitive1() { compileAndTest(DeliteGPUReferencePrimitive1Runner, CHECK_MULTILOOP); }
  def testDeliteGPUReferencePrimitive2() { compileAndTest(DeliteGPUReferencePrimitive2Runner, CHECK_MULTILOOP); }
}
