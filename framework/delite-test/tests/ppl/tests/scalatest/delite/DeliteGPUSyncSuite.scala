package ppl.tests.scalatest.delite

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._


object DeliteGPUSyncRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPUSync
trait DeliteGPUSync extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val x = DeliteArrayBuffer.fromFunction(10){ i => 0.0 }.mutable
    x.forIndices { i => x(i) = x(i) + i } //mutation on GPU

    val y1 = x.filter(_ < 3.0)
    val y2 = x.filter(_ >= 3.0)
    collect(y1.length == 3)
    collect(y2.length == 7)

    x.insert(0,0.5)
    collect(x(0) == 0.5)
    collect(x(1) == 0.0)
    collect(x.length == 11)
    
    mkReport
  }
}

// mutation on CPU inside loop
object DeliteGPULoopSync1Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync1
trait DeliteGPULoopSync1 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 

    val v = DeliteArrayBuffer.fromFunction(10){ i => 0.0 }.mutable
    var i = 0
    while (i < v.length) {
      v(i) = i * 1.5 //mutation on CPU
      val sum = v.reduce( _ + _ )(0.0)
      collect(sum == 1.5*i*(i+1)/2)
      i += 1
    }
    
    mkReport
  }
}

// mutation on GPU inside loop, CPU use is in the same scope
object DeliteGPULoopSync2Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync2
trait DeliteGPULoopSync2 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 

    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    var i = 0
    while (i < 3) {
      v.forIndices { j => v(j) = v(j) + 1.0 } //mutation on GPU
      val sum1 = v.reduce(_ + _)(0.0) //sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) //sum on CPU
      collect(sum1 == sum2)
      i += 1
    }
    
    mkReport
  }
}

// mutation on GPU inside loop, CPU use is in the outer scope
object DeliteGPULoopSync3Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync3
trait DeliteGPULoopSync3 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    var i = 0
    while (i < 3) {
      v.forIndices { j => v(j) = v(j) + 1.0 } //mutation on GPU
      i += 1
    }
    val sum1 = v.reduce(_ + _)(0.0) //sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
    collect(sum1 == sum2)   
  
    mkReport
  }
}

// mutation on GPU inside nested-loop, CPU use is in the inner scope
object DeliteGPULoopSync4Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync4
trait DeliteGPULoopSync4 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 

    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        v.forIndices { j => v(j) = v(j) + 1.0 } 
        k += 1
      }
      val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1
    }  
  
    mkReport
  }
}

// mutation on GPU inside nested-loop, CPU use is in the outer scope
object DeliteGPULoopSync5Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync5
trait DeliteGPULoopSync5 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 
    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        v.forIndices { j => v(j) = v(j) + 1.0 }
        k += 1
      }
      i += 1
    }
    val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU    
    collect(sum1 == sum2)
  
    mkReport
  }
}

// anti-dependency: mutation on GPU inside nested-loop, CPU use is in the inner scope
object DeliteGPULoopSync6Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync6
trait DeliteGPULoopSync6 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 
    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable 
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        v.forIndices { j => v(j) = v(j) + k*1.5 }
        k += 1
      }
      val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1
    }  
 
    mkReport
  }
}

// anti-dependency: mutation on GPU inside nested-loop, CPU use is in the outer scope
object DeliteGPULoopSync7Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync7
trait DeliteGPULoopSync7 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 
    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable 
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        v.forIndices { j => v(j) = v(j) + k*1.5}
        k += 1
      }
      i += 1
    }
    val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU    
    collect(sum1 == sum2)
  
    mkReport
  }
}

// test anti-dependency
object DeliteGPULoopSync8Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync8
trait DeliteGPULoopSync8 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 
    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    var i = 0
    while(i < 3) {
      v.forIndices { j => v(j) = v(j) + i*1.5} // mutation on GPU, using i
      val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1 // mutation on CPU
    } 
  
    mkReport
  }
}

// test multiple mutators
object DeliteGPULoopSync9Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync9
trait DeliteGPULoopSync9 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 
    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    var i = 0
    while(i < 3) {
      v.forIndices { j => v(j) = v(j) + i*1.5 }
      v.forIndices { j => v(j) = v(j) + i*1.5 }
      val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
      i += 1
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1
    }
  
    mkReport
  }
}

// test conditional mutators
object DeliteGPULoopSync10Runner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteGPULoopSync10
trait DeliteGPULoopSync10 extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = { 
    val v = DeliteArrayBuffer.fromFunction(5){ i => 2.5*i }.mutable
    
    def splitCondition(pred: Rep[Boolean]) = {
      if (pred) {
        v.forIndices { j => v(j) = v(j) + 1.0 } // mutation on GPU
      }
      else {
        v(3) = 3.7  // mutation on CPU
      }

      val sum1 = v.reduce(_ + _)(0.0) // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      
      v.forIndices { j => v(j) = v(j) + 1.0 }
      val sum3 = v.reduce(_ + _)(0.0) // sum on GPU
      val sum4 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum3 == sum4)
    }

    splitCondition(unit(0.0).AsInstanceOf[Int] == 0)
    splitCondition(unit(1.0).AsInstanceOf[Int] == 0)
  
    mkReport
  }
}

class DeliteGPUSyncSuite extends DeliteSuite {
  def testDeliteGPUSync() { compileAndTest(DeliteGPUSyncRunner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync1() { compileAndTest(DeliteGPULoopSync1Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync2() { compileAndTest(DeliteGPULoopSync2Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync3() { compileAndTest(DeliteGPULoopSync3Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync4() { compileAndTest(DeliteGPULoopSync4Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync5() { compileAndTest(DeliteGPULoopSync5Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync6() { compileAndTest(DeliteGPULoopSync6Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync7() { compileAndTest(DeliteGPULoopSync7Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync8() { compileAndTest(DeliteGPULoopSync8Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync9() { compileAndTest(DeliteGPULoopSync9Runner, CHECK_MULTILOOP); }
  def testDeliteGPULoopSync10() { compileAndTest(DeliteGPULoopSync10Runner, CHECK_MULTILOOP); }
}
