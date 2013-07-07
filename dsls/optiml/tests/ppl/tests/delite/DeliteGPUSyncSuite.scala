package ppl.tests.scalatest.delite

import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

object DeliteGPUSyncRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUSync
trait DeliteGPUSync extends DeliteTestModule with OptiMLApplication {
  def main() = {
    
    val x = Vector.zeros(10).mutable

    for(i <- (0::10)) { x(i) = x(i) + i }  // mutation on GPU

    val (y1,y2) = x.partition(i => i < 3.0)
    collect(y1.length == 3)
    collect(y2.length == 7)

    x.insert(0,0.5)
    collect(x(0)==0.5)
    collect(x.length == 11)
    
    mkReport
  }
}

// mutation on CPU inside loop
object DeliteGPULoopSync1Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync1
trait DeliteGPULoopSync1 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.zeros(10).mutable
    var i = 0 
    while(i < v.length) {
      v(i) = i * 1.0   // mutation on CPU
      val sum = v.sum  // sum on GPU
      collect(sum == 1.0*i*(i+1)/2)
      i += 1
    }   
  }
}

// mutation on GPU inside loop, CPU use is in the same scope
object DeliteGPULoopSync2Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync2
trait DeliteGPULoopSync2 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable
    var i = 0
    while(i < 3) {
      for(j <- (0::5)) { v(j) = v(j) + 1.0} // mutation on GPU
      val sum1 = v.sum // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)   
      i += 1
    }
  }
}

// mutation on GPU inside loop, CPU use is in the outer scope
object DeliteGPULoopSync3Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync3
trait DeliteGPULoopSync3 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable
    var i = 0
    while(i < 3) {
      for(j <- (0::5)) { v(j) = v(j) + 1.0} // mutation on GPU
      i += 1
    }
    val sum1 = v.sum // sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
    collect(sum1 == sum2)   
  }
}

// mutation on GPU inside nested-loop, CPU use is in the inner scope
object DeliteGPULoopSync4Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync4
trait DeliteGPULoopSync4 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable 
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        for(j <- (0::5)) { v(j) = v(j) + 1.0}
        k += 1
      }
      val sum1 = v.sum // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1
    }  
  }
}

// mutation on GPU inside nested-loop, CPU use is in the outer scope
object DeliteGPULoopSync5Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync5
trait DeliteGPULoopSync5 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable 
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        for(j <- (0::5)) { v(j) = v(j) + 1.0}
        k += 1
      }
      i += 1
    }
    val sum1 = v.sum // sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU    
    collect(sum1 == sum2)
  }
}

// anti-dependency: mutation on GPU inside nested-loop, CPU use is in the inner scope
object DeliteGPULoopSync6Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync6
trait DeliteGPULoopSync6 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable 
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        for(j <- (0::5)) { v(j) = v(j) + k*1.0}
        k += 1
      }
      val sum1 = v.sum // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1
    }  
  }
}

// anti-dependency: mutation on GPU inside nested-loop, CPU use is in the outer scope
object DeliteGPULoopSync7Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync7
trait DeliteGPULoopSync7 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable 
    var i = 0
    while (i < 2) {
      var k = 0
      while(k < 3) {
        for(j <- (0::5)) { v(j) = v(j) + k*1.0}
        k += 1
      }
      i += 1
    }
    val sum1 = v.sum // sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU    
    collect(sum1 == sum2)
  }
}

// test anti-dependency
object DeliteGPULoopSync8Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync8
trait DeliteGPULoopSync8 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable
    var i = 0
    while(i < 3) {
      for(j <- (0::5)) { v(j) = v(j) + i*1.0} // mutation on GPU, using i
      val sum1 = v.sum // sum on GPU
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1 // mutation on CPU
    } 
  }
}

// test multiple mutators
object DeliteGPULoopSync9Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync9
trait DeliteGPULoopSync9 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable
    var i = 0
    while(i < 3) {
      for(j <- (0::5)) { v(j) = v(j) + i*1.0}
      for(j <- (0::5)) { v(j) = v(j) + i*1.0}
      val sum1 = v.sum // sum on GPU
      i += 1
      val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
      collect(sum1 == sum2)
      i += 1
    }
  }
}

// test conditional mutators
object DeliteGPULoopSync10Runner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPULoopSync10
trait DeliteGPULoopSync10 extends DeliteTestModule with OptiMLApplication {
  def main() = { 
    val v = Vector.rand(5).mutable
    var i = 0
    if(i == 1) {
      for(j <- (0::v.length)) { v(j) = v(j) + 1.0} // mutation on GPU
    }
    else {
      v(3) = 3.7  // mutation on CPU
    }
    val sum1 = v.sum // sum on GPU
    val sum2 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
    collect(sum1 == sum2)
    
    for(j <- (0::v.length)) { v(j) = v(j) + 1.0}
    val sum3 = v.sum // sum on GPU
    val sum4 = v(0) + v(1) + v(2) + v(3) + v(4) // sum on CPU
    collect(sum3 == sum4)
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
}
