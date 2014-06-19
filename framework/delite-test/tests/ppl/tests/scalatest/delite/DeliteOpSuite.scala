package ppl.tests.scalatest.delite

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._
import scala.virtualization.lms.common.Record

/* Tests the generated code functionality for Delite ops, using core Delite data structures.
*/

trait DeliteTestBase extends DeliteTestModule with DeliteTestDSLApplication {
  def Complex(re: Rep[Double], im: Rep[Double]) = new Record { val real = re; val imag = im }

  type Chars = Record { val a: Char; val b: Char; val c: Char }
  def Chars(a0: Rep[Char], b0: Rep[Char], c0: Rep[Char]): Rep[Chars] = new Record { val a = a0; val b = b0; val c = c0 }

  def Single(a0: Rep[Int]) = new Record { val a = a0 }

  def collectArray[A:Manifest](arr: Rep[DeliteArray[A]], expectedLength: Rep[Int], expectedValues: Rep[Int] => Rep[A]) {
    collect(arr.length == expectedLength)
    for (i <- 0 until arr.length) {
      collect(arr(i) == expectedValues(i))
    }
  }

  def collectBuf[A:Manifest](buf: Rep[DeliteArrayBuffer[A]], expectedLength: Rep[Int], expectedValues: Rep[Int] => Rep[A]) {
    collect(buf.length == expectedLength)
    for (i <- 0 until buf.length) {
      collect(buf(i) == expectedValues(i))
    }
  }
}

object DeliteMapRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteMap
trait DeliteMap extends DeliteTestBase {
  def main() = {

    val a = DeliteArray.fromFunction(1000){ i => 0.0 }
    val a2 = a map { e => 10 }
    collectArray(a2, 1000, i => 10)

    val v = DeliteArrayBuffer.fromFunction(1000){ i => 0.0 }
    val v2 = v map { e => 10 }
    collectBuf(v2, 1000, i => 10)

    val vs = DeliteArrayBuffer.fromFunction(500){ i => Complex(0.0, 0.0) }
    val vs2 = vs map { e => Complex(e.real + 5.0, e.imag - 5.0) }
    collectBuf(vs2, 500, i => Complex(5, -5))

    val va = DeliteArrayBuffer.fromFunction(500){ i => Single(i) }
    println(va) //force creation of va
    collectBuf(va.map(_.a), 500, i => i)

    val ve = DeliteArrayBuffer.fromFunction(0){ i => 0 }
    val ve2 = ve map { e => 1 }
    collectBuf(ve2, 0, i => 1)
    
    mkReport
  }
}

object DeliteFlatMapRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteFlatMap
trait DeliteFlatMap extends DeliteTestBase {
  def main() = {

    val a = DeliteArray.fromFunction(1000){ i => i}
    val a2 = a flatMap { i => DeliteArray.fromFunction(10){ j => i }}
    collectArray(a2, 10000, i => i/10)

    val v = DeliteArrayBuffer.fromFunction(1000){ i => i }
    val v2 = v flatMap { i => DeliteArrayBuffer.fromFunction(10){ j => i }}
    collectBuf(v2, 10000, i => i/10)

    val vm = DeliteArrayBuffer.fromFunction(100){ i => i }
    val vm2 = vm flatMap { i => DeliteArrayBuffer(DeliteArray.fromFunction(2){ j => 0 }, 2).map{ j => j + i }}
    collectBuf(vm2, 200, i => i/2)

    val ve = DeliteArrayBuffer.fromFunction(0){ i => 0 }
    val ve2 = ve flatMap { i => DeliteArrayBuffer.fromFunction(0) { j => i }}
    collectBuf(ve2, 0, i => i)

    mkReport
  }
}

object DeliteZipRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteZip
trait DeliteZip extends DeliteTestBase {
  def main() = {

    val a1 = DeliteArray.fromFunction(1000){ i => 1.0 }
    val a2 = DeliteArray.fromFunction(1000){ i => 0.0 } map { e => 2.0 }
    val a3 = a1.zip(a2){ _ + _ }
    collectArray(a3, 1000, i => 3.0)

    val v1 = DeliteArrayBuffer.fromFunction(1000){ i => 1.0 }
    val v2 = DeliteArrayBuffer.fromFunction(1000){ i => 0.0 } map { e => 2.0 }
    val v3 = v1.zip(v2){ _ + _ }
    collectBuf(v3, 1000, i => 3.0)

    val ve1 = DeliteArrayBuffer.fromFunction(0){ i => 0.0 }
    val ve2 = DeliteArrayBuffer.fromFunction(0){ i => 0.0 }
    val ve3 = ve1.zip(ve2){ _ - _ }
    collectBuf(ve3, 0, i => 0.0)
    
    // TODO: what is expected behavior when collections aren't the same size? (should be an exception)
    
    mkReport
  }
}

object DeliteReduceRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteReduce
trait DeliteReduce extends DeliteTestBase {
  def main() = {

    collect(DeliteArray.fromFunction(1000){ i => 0 }.reduce( _ + _, 0) == 0)

    val v = DeliteArrayBuffer(DeliteArray.fromFunction(1000){ i => 0 } , 1000)
    collect(v.reduce( _ + _ )(0) == 0)

    val ve = DeliteArrayBuffer.fromFunction(0){ i => 0 }
    collect(ve.reduce( _ + _ )(0) == 0)

    mkReport
  }
}

// TODO: Current GPU reduce only works for commutative reduce operators.
// Enable this test when this gets fixed.
object DeliteReduce2Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteReduce2
trait DeliteReduce2 extends DeliteTestBase {
  def main() = {

    val v = DeliteArrayBuffer.fromFunction(10){ i => i+5 }
    val s = v.reduce{ (a,b) => b }(0)
    collect(s == 14)

    mkReport
  }
}


object DeliteMapReduceRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteMapReduce
trait DeliteMapReduce extends DeliteTestBase {
  def main() = {

    val a = DeliteArray.fromFunction(1000){ i => i}
    collect(a.reduce( _ + _, 0) == 499500)

    val v = DeliteArrayBuffer.fromFunction(1000){ i => i } 
    collect(v.reduce( _ + _ )(0) == 499500)

    val v2 = DeliteArrayBuffer.fromFunction(500){ i => Complex(i, 0-i) }    
    val x2 = v2.reduce{ (a,b) => Complex(a.real + b.real, a.imag + b.imag) }(Complex(0,0))
    collect(x2 == Complex(124750, -124750))

    mkReport
  }
}

object DeliteFilterRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteFilter
trait DeliteFilter extends DeliteTestBase {
  def main() = {

    //TODO: doesn't work in CUDA codegen
    // val a1 = DeliteArray.fromFunction(50){ i => i }
    // val a2 = a1.filter(_ % 2 == 1)
    // collectArray(a2, 25, i => a1(1+i*2))

    val v1 = DeliteArrayBuffer.fromFunction(100){ i => i }
    val v2 = v1.filter(_ % 2 == 1)
    collectBuf(v2, 50, i => v1(1+i*2))

    val ve = DeliteArrayBuffer.fromFunction(0){ i => 0 }
    val ve2 = ve.filter(_ % 2 == 1)
    collectBuf(ve2, 0, i => 0)
    
    mkReport
  }
}

object DeliteForeachRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteForeach
trait DeliteForeach extends DeliteTestBase {
  def main() = {

    val v = DeliteArrayBuffer.fromFunction(10){ i => i }
    for (e <- v) {
      if ((e > 0) && (e < v.length-1)) {
        collect(v(e+1) - v(e-1) == 2)
      }
    }

    val ve = DeliteArrayBuffer.fromFunction(0){ i => 0 }
    for (e <- ve) {
      collect(false) //shouldn't be executed
    }

    val vb = DeliteArrayBuffer.fromFunction(500){ i => Chars('a', 'b', 'c') }
    val arr = NewArray[Chars](vb.length) //an AoS array
    vb forIndices { i => arr(i) = vb(i) } //force struct boxing
    val vb2 = DeliteArrayBuffer.fromFunction(vb.length) { i => arr(i) } //force struct unboxing
    collectBuf(vb2.map(_.a), 500, i => unit('a'))
    collectBuf(vb2.map(_.b), 500, i => unit('b'))
    collectBuf(vb2.map(_.c), 500, i => unit('c'))
    
    mkReport
  }
}

object DeliteZipWithReduceTupleRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteZipWithReduceTuple
trait DeliteZipWithReduceTuple extends DeliteTestBase {
  def main() = {

    val v = DeliteArrayBuffer.fromFunction(10){ i => i+5 }
    val i = DeliteArrayBuffer.fromFunction(10){ i => i+1 }

    val s = v.zip(i){ (a,b) => (a,b) }.reduce{ (a,b) => (a._1+b._1, a._2+b._2) }(make_tuple2((0,0)))
    collect(s._1 == 95)
    collect(s._2 == 55)

    //val maxWithIndex = v.zip(i){ (a,b) => (a,b) }.reduce{ (a,b) => if (a._1 < b._1) a else b }(unit(null)) //rFunc isn't separable

    mkReport
  }
}

object DeliteGroupByRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGroupBy
trait DeliteGroupBy extends DeliteTestBase {
  def main() = {
    
    val res = DeliteArrayBuffer.fromFunction(1000){ i => i } groupBy { i => i % 2 == 0 }
    collect(res.size == 2)
    collectBuf(res(true), 500, i => 2*i)
    collectBuf(res(false), 500, i => 2*i+1)

    val res2 = DeliteArrayBuffer.fromFunction(1000*1000){ i => i/1000 } groupBy { i => i }
    collect(res2.size == 1000)
    for (i <- 0 until res2.size) {
      collectBuf(res2(i), 1000, j => i)
    }

    val res3 = DeliteArrayBuffer.fromFunction(0){ i => i } groupBy { i => i }
    collect(res3.size == 0)

    val res4 = DeliteArrayBuffer.fromFunction(1000*1000){ i => new Record{ val a = infix_/(i,1000); val b = i }} groupBy { _.a }
    collect(res4.size == 1000)
    for (i <- 0 until res4.size) {
      collectBuf(res4(i).map(_.a), 1000, j => i)
    }

    mkReport
  }
}

object DeliteGroupByReduceRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGroupByReduce
trait DeliteGroupByReduce extends DeliteTestBase {
  def main() = {
    
    val a = DeliteArray.fromFunction(1000){ i => i } groupByReduce(i => i % 2 == 0, i => i, (a:Rep[Int],b:Rep[Int]) => a + b)
    collect(a.size == 2)
    collect(a(true) == 249500)
    collect(a(false) == 250000)

    val res = DeliteArrayBuffer.fromFunction(1000){ i => i } groupByReduce(i => i % 2 == 0, i => i, (a:Rep[Int],b:Rep[Int]) => a + b)
    collect(res.size == 2)
    collect(res(true) == 249500)
    collect(res(false) == 250000)

    val res2 = DeliteArrayBuffer.fromFunction(0){ i => i } groupByReduce(i => i, i => i, (a:Rep[Int],b:Rep[Int]) => a + b)
    collect(res2.size == 0)

    val res3 = DeliteArrayBuffer.fromFunction(1000){ i => new Record{ val a = infix_/(i,10); val b = i }} groupByReduce(_.a, _.b, (a:Rep[Int],b:Rep[Int]) => a)
    collect(res3.size == 100)
    collectArray(res3.values, 100, i => i*10)

    mkReport
  }
}

object DeliteNestedMapRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteNestedMap
trait DeliteNestedMap extends DeliteTestBase {
  def main() = {
    
    val a = DeliteArray.fromFunction(1){ i => i } map { e => 
      DeliteArray.fromFunction(1000){ j => 0 } map { f => 10 + e }
    }
    collectArray(a(0), 1000, i => 10)

    val res = DeliteArrayBuffer.fromFunction(1){ i => i } map { e => 
      DeliteArrayBuffer.fromFunction(1000){ j => 0 } map { f => 10 + e }
    }
    collectBuf(res(0), 1000, i => 10)

    val res2 = DeliteArrayBuffer.fromFunction(1){ i => i } map { e => 
      DeliteArrayBuffer.fromFunction(0){ j => 0 } map { f => 10 + e }
    }
    collectBuf(res2(0), 0, i => 10)
    
    mkReport
  }
}

object DeliteNestedZipRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteNestedZip
trait DeliteNestedZip extends DeliteTestBase {
  def main() = {
    
    val a = DeliteArray.fromFunction(1){ i => i} map { e => 
      val a1 = DeliteArray.fromFunction(1000){ i => 1.0 + e }
      val a2 = DeliteArray.fromFunction(1000){ i => i } map { f => 2 + e }
      a1.zip(a2){ _ + _ }
    }
    collectArray(a(0), 1000, i => 3.0)

    val res = DeliteArrayBuffer.fromFunction(1){ i => i } map { e => 
      val v1 = DeliteArrayBuffer.fromFunction(1000){ i => 1.0 + e }
      val v2 = DeliteArrayBuffer.fromFunction(1000){ i => i } map { f => 2 + e }
      v1.zip(v2){ _ + _ }
    }
    collectBuf(res(0), 1000, i => 3.0)

    val res2 = DeliteArrayBuffer.fromFunction(1){ i => i } map { e => 
      val ve1 = DeliteArrayBuffer.fromFunction(0){ i => 1.0 + e }
      val ve2 = DeliteArrayBuffer.fromFunction(0){ i => i } map { f => 2 + e }
      ve1.zip(ve2){ _ + _ }
    }
    collectBuf(res2(0), 0, i => 3.0)
    
    mkReport
  }
}

object DeliteNestedReduceRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteNestedReduce
trait DeliteNestedReduce extends DeliteTestBase {
  def main() = {
    
    val a = DeliteArray.fromFunction(1){ i => i } map { e => 
      DeliteArray.fromFunction(e){ i => 0 }.reduce( _ + _, 0)
    }
    collect(a(0) == 0)

    val res = DeliteArrayBuffer.fromFunction(1){ i => i } map { e => 
      DeliteArrayBuffer.fromFunction(e){ i => 0 } .reduce( _ + _ )(0)
    }
    collect(res(0) == 0)
    
    mkReport
  }
}

object DeliteNestedMapReduceRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteNestedMapReduce
trait DeliteNestedMapReduce extends DeliteTestBase {
  def main() = {
    
    val a = DeliteArray.fromFunction(1){ i => i } map { e => 
      DeliteArray.fromFunction(1000){ i => i + e }.reduce( _ + _, 0)
    }
    collect(a(0) == 499500)

    val res = DeliteArrayBuffer.fromFunction(1){ i => i } map { e => 
      DeliteArrayBuffer.fromFunction(1000){ i => i + e }.reduce( _ + _ )(0)
    }
    collect(res(0) == 499500)

    mkReport
  }
}

object DeliteNestedForeachRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteNestedForeach
trait DeliteNestedForeach extends DeliteTestBase {
  def main() = {

    DeliteArrayBuffer.fromFunction(1){ i => i } foreach { i => 
      val v = DeliteArrayBuffer.fromFunction(10){ i => i }
      for (e <- v) {
        if ((e > 0) && (e < v.length-1)) {
          collect(v(e+1) - v(e-1) == 2)
        }
      }
    }

    DeliteArrayBuffer.fromFunction(1){ i => i } foreach { i =>
      val ve = DeliteArrayBuffer.fromFunction(0){ i => i }
      for (e <- ve) {
        collect(false) //shouldn't be executed
      }
    }
    
    mkReport
  }
}

object DeliteIfThenElseRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteIfThenElse
trait DeliteIfThenElse extends DeliteTestBase {
  def main() = {

    val p = unit(1.0).AsInstanceOf[Int] //trickery to prevent constant propagation
    if (p == 1) {
      var x1 = 99
      collect(x1 == 99)    
    }
    else {
      collect(false)
    }

    // check a conditional with an effect in a branch that returns a constant (previously caused a DEG parse error)
    val z = 
      if (p > 1) {
        13
      }
      else {
        println("cond_check")
        3
      }
    collect(z == 3)

    mkReport
  }
}

object DeliteHorizontalElemsRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteHorizontalElems
trait DeliteHorizontalElems extends DeliteTestBase {
  def main() = {
    
    var i = 0
    while (i < 10) { //small collection sizes, test with multiple threads!
      val size = i
      //each line should fuse vertically (along with v), and all the lines should fuse horizontally
      val v = DeliteArrayBuffer.fromFunction(size){ j => 1 }
      collect(v.reduce( _ + _ )(0) == size)
      collect(v.filter(_ > 1).map(e => 1).reduce(_ + _)(0) == 0)
      collect(DeliteArrayBuffer.fromFunction(size)(j => j).filter(_ % 2 == 0).length == (size+1)/2)
      collect(v.filter(_ > 1).length == 0)
      collect(v.map(e => e + 1).length == size)
      i += 1
    }
    
    mkReport
  }
}

object DeliteFileReaderRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteFileReader
trait DeliteFileReader extends DeliteTestBase {
  def main() = {
    val path = sys.env.get("DELITE_HOME").map(_ + "/").getOrElse("") + "framework/delite-test/tests/ppl/tests/scalatest/delite/test.txt"
    val numLines = 3
    val numElems = 6
    val elem = unit("a")

    val a1 = DeliteNewFileReader.readLines(path){ line => 
      val fields = line.split(" ")
      fields(0)
    }
    collectArray(a1, numLines, i => elem)

    val a2 = DeliteNewFileReader.readLines(path){ line => 
      val fields = line.split(" ")
      (fields(0), fields(0))
    }
    collectArray(a2, numLines, i => (elem, elem))

    val a3 = DeliteNewFileReader.readLines(path){ line => 
      val fields = line.split(" ")
      DeliteArray.fromFunction(fields.length)(i => fields(i))
    }
    collect(a3.length == numLines)
    for (i <- 0 until a3.length) {
      collectArray(a3(i), i+1, i => elem)
    }

    val a4 = DeliteNewFileReader.readLinesFlattened(path){ line => 
      val fields = line.split(" ")
      DeliteArray.fromFunction(fields.length)(i => fields(i))
    }
    collectArray(a4, numElems, i => elem)

    val a5 = DeliteNewFileReader.readLinesFlattened(path){ line => 
      val fields = line.split(" ")
      DeliteArray.fromFunction(fields.length)(i => (fields(i), fields(i)))
    }
    collectArray(a5, numElems, i => (elem,elem))
    
    mkReport
  }
}

class DeliteOpSuite extends DeliteSuite {
  def testDeliteMap() { compileAndTest(DeliteMapRunner, CHECK_MULTILOOP) }
  def testDeliteFlatMap() { compileAndTest(DeliteFlatMapRunner) }
  def testDeliteZip() { compileAndTest(DeliteZipRunner, CHECK_MULTILOOP) }
  def testDeliteReduce() { compileAndTest(DeliteReduceRunner, CHECK_MULTILOOP) }
  //def testDeliteReduce2() { compileAndTest(DeliteReduce2Runner, CHECK_MULTILOOP) }
  def testDeliteMapReduce() { compileAndTest(DeliteMapReduceRunner, CHECK_MULTILOOP) }
  def testDeliteFilter() { compileAndTest(DeliteFilterRunner, CHECK_MULTILOOP) }
  def testDeliteForeach() { compileAndTest(DeliteForeachRunner) }
  def testDeliteZipWithReduceTuple() { compileAndTest(DeliteZipWithReduceTupleRunner, CHECK_MULTILOOP) }
  def testDeliteNestedMap() { compileAndTest(DeliteNestedMapRunner) }
  def testDeliteHorizontalElems() { compileAndTest(DeliteHorizontalElemsRunner, CHECK_MULTILOOP) }
  def testDeliteNestedZip() { compileAndTest(DeliteNestedZipRunner) }
  def testDeliteNestedReduce() { compileAndTest(DeliteNestedReduceRunner, CHECK_MULTILOOP) }
  def testDeliteNestedMapReduce() { compileAndTest(DeliteNestedMapReduceRunner, CHECK_MULTILOOP) }
  def testDeliteNestedForeach() { compileAndTest(DeliteNestedForeachRunner) }
  def testDeliteIfThenElse() { compileAndTest(DeliteIfThenElseRunner) }
  def testDeliteGroupBy() { compileAndTest(DeliteGroupByRunner) }
  def testDeliteGroupByReduce() { compileAndTest(DeliteGroupByReduceRunner) }
  def testDeliteFileReader() { compileAndTest(DeliteFileReaderRunner) }
}
