package ppl.tests.multiarray

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.delite.MultiArrayGenException
import ppl.delite.framework.transform._


trait DeliteMultiArrayTestbenchRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner 
  with MultiArrayTransform 

trait DeliteMultiArrayTestbench extends DeliteTestModule with DeliteTestDSLApplication 
  with DeliteMultiArrayOps {
  type MultiArray[T] = DeliteMultiArray[T]
  type Array1D[T] = DeliteArray1D[T]
  type Array2D[T] = DeliteArray2D[T]
  type Array3D[T] = DeliteArray3D[T]
  type Array4D[T] = DeliteArray4D[T]
  type Array5D[T] = DeliteArray5D[T]

  implicit def multiArrayManifest[T:Manifest] = manifest[DeliteMultiArray[T]]
  implicit def array1DManifest[T:Manifest] = manifest[DeliteArray1D[T]]
  implicit def array2DManifest[T:Manifest] = manifest[DeliteArray2D[T]]
  implicit def array3DManifest[T:Manifest] = manifest[DeliteArray3D[T]]
  implicit def array4DManifest[T:Manifest] = manifest[DeliteArray4D[T]]
  implicit def array5DManifest[T:Manifest] = manifest[DeliteArray5D[T]]
  
  implicit def array1DtoMultiArray[T:Manifest](x: Rep[DeliteArray1D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array2DtoMultiArray[T:Manifest](x: Rep[DeliteArray2D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array3DtoMultiArray[T:Manifest](x: Rep[DeliteArray3D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array4DtoMultiArray[T:Manifest](x: Rep[DeliteArray4D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array5DtoMultiArray[T:Manifest](x: Rep[DeliteArray5D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
}

/*object SinglyNestedMultiArrayRunner extends DeliteMultiArrayTestbenchRunner with SinglyNestedMultiArray
trait SinglyNestedMultiArray extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => Array1D[Float](3)}
    vec foreach {k => collect(k.reduce(0.0f){(a,b) => a + b} == 0.0f)}

    mkReport
  }
}

object NullMultiArrayRunner extends DeliteMultiArrayTestbenchRunner with NullMultiArray 
trait NullMultiArray extends DeliteMultiArrayTestbench {
  def main() = {
    
  }
}

object MultiArrayUpdateRunner extends DeliteMultiArrayTestbenchRunner with MultiArrayUpdate 
trait MultiArrayUpdate extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D[MultiArray[Int]](3)
    vec forIndices {i => vec(i) = Array1D.fromFunction(3){j => j}}

    collect(vec(0).reduce(0){_+_} == 3)

    mkReport
  }
}

object IllegalMultiArrayUpdateRunner extends DeliteMultiArrayTestbenchRunner with IllegalMultiArrayUpdate 
trait IllegalMultiArrayUpdate extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D[MultiArray[Int]](3)
    vec forIndices {i => vec(i) = Array1D.fromFunction(3){j => j}}

    // Disallowed - all nested arrays must have same dimensionality
    vec(1) = Array2D.fromFunction(2, 2){(i,j) => i + j}

    collect(vec(0).reduce(0){_+_} == 3)

    mkReport
  }
}

object IllegalMutationRunner extends DeliteMultiArrayTestbenchRunner with IllegalMutation
trait IllegalMutation extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array2D.fromFunction(2, 2){(i,j) => i + j}
    
    // Not allowed - vec is immutable
    vec(0,0) = 10

    collect(vec(0,0) == 0)

    mkReport
  }
}*/

object New1DRunner extends DeliteMultiArrayTestbenchRunner with New1DTest
trait New1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D[Int](3)
    collect( vec.mkString() == "0,0,0" )
    mkReport
  }
}

// Should still be able to infer DeliteArray1D[DeliteArray1D[Int]] with rank info 
object New1DNullRunner extends DeliteMultiArrayTestbenchRunner with New1DNullTest
trait New1DNullTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D[Array1D[Int]](3)
    collect( vec.mkString() == "null,null,null" )
    mkReport
  }
}

object FromFunction1DRunner extends DeliteMultiArrayTestbenchRunner with FromFunction1DTest
trait FromFunction1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(3){i => i}
    collect( vec.mkString() == "0,1,2" )
    mkReport
  }
}

object Update1DRunner extends DeliteMultiArrayTestbenchRunner with Update1DTest
trait Update1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D[Array1D[Int]](3)
    vec(0) = Array1D.fromFunction(3){i => i}
    collect( vec(0).mkString() == "0,1,2" )
    mkReport
  }
}

object Map1DRunner extends DeliteMultiArrayTestbenchRunner with Map1DTest
trait Map1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(5){i => i}
    val vec2 = vec.map{x => x * 2}
    println(vec.mkString()); println(vec2.mkString())
    collect(vec2(4) == 8)
    mkReport
  }
}

object Zip1DRunner extends DeliteMultiArrayTestbenchRunner with Zip1DTest
trait Zip1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(5){i => i}
    val vec2 = vec.map{x => vec.length - x}
    val vec3 = vec2.zip(vec){(a,b) => a + b}
    println(vec.mkString()); println(vec2.mkString()); println(vec3.mkString())
    collect(vec3(4) == 5)
    mkReport
  }
}

object Reduce1DRunner extends DeliteMultiArrayTestbenchRunner with Reduce1DTest
trait Reduce1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(5){i => i}
    val v = vec.reduce(unit(0)){(a,b) => a + b}
    collect(v == 10)
    mkReport
  }
}

object Foreach1DRunner extends DeliteMultiArrayTestbenchRunner with Foreach1DTest
trait Foreach1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.map{i => i * 20}
    vec2.foreach{k => println(vec2(k/20) + ", " + k); collect(vec2(k/20) == k) }
    mkReport
  }
}

object ForeachNoEffect1DRunner extends DeliteMultiArrayTestbenchRunner with ForeachNoEffect1DTest
trait ForeachNoEffect1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => 10 - i}
    vec.foreach{x => val v = Array1D[Int](x); v(0); println(x) }
    println(vec.mkString())
    collect(vec.reduce(unit(0)){(a,b) => a + b} == 55)
    mkReport
  }
}

object ForIndices1DRunner extends DeliteMultiArrayTestbenchRunner with ForIndices1DTest
trait ForIndices1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.map{i => i * 20}
    vec2.forIndices{i => collect(vec2(i) == i*20) }
    mkReport 
  }
}

object ForIndicesNoEffect1DRunner extends DeliteMultiArrayTestbenchRunner with ForIndicesNoEffect1DTest
trait ForIndicesNoEffect1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => 10 - i}
    vec.forIndices{i => val v = Array1D[Int](i+1); v(0); println(i) }
    collect(vec.reduce(unit(0)){(a,b) => a + b} == 55)
    mkReport
  }
}

// Permuting a 1D array is just a copy, just confirming that it works here
object Permute1DRunner extends DeliteMultiArrayTestbenchRunner with Permute1DTest
trait Permute1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = dmultia_permute(vec, Seq(0))
    vec.forIndices{i => collect(vec(i) == vec2(i)) }
    mkReport
  }
}

object FilterRunner extends DeliteMultiArrayTestbenchRunner with FilterTest
trait FilterTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}
    val v2 = vec.filter{x => x % 2 == 0}
    collect(v2.reduce(unit(0)){(a,b) => a + b} == 20)
    mkReport
  }
}

object FlatMapRunner extends DeliteMultiArrayTestbenchRunner with FlatMapTest
trait FlatMapTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}
    val v2 = vec.flatMap{x => Array1D.fromFunction(x){i => i} }
    println(vec.mkString()); println(v2.mkString())
    collect(v2.reduce(unit(0)){(a,b) => a + b} == 165)
    mkReport
  }
}

object SortRunner extends DeliteMultiArrayTestbenchRunner with SortTest
trait SortTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.sort
    val vec3 = vec.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(vec.mkString()); println(vec2.mkString()); println(vec3.mkString())
    collect(vec(0) == 0)
    mkReport
  }
} 

object StringSplitRunner extends DeliteMultiArrayTestbenchRunner with StringSplitTest
trait StringSplitTest extends DeliteMultiArrayTestbench {
  def main() = {
    val str = "4,3,2,1,0"
    val vec = Array1D.splitString(str,",").map{_.toInt}
    println(vec.mkString())
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == unit(5) - i)}
    mkReport
  }
}

object InsertRunner extends DeliteMultiArrayTestbenchRunner with InsertTest
trait InsertTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    vec.insert(0, -1)
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == i - 1) }
    mkReport
  }
}
object AppendRunner extends DeliteMultiArrayTestbenchRunner with AppendTest
trait AppendTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    vec := 4
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == i)}
    mkReport
  }
}

object InsertAll1DRunner extends DeliteMultiArrayTestbenchRunner with InsertAll1DTest
trait InsertAll1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    val vec2 = Array1D.fromFunction(4){i => i + 4}
    vec ::= vec2
    collect(vec.length == 8)
    vec.forIndices{i => collect(vec(i) == i)}
    mkReport
  }
}

object Remove1DRunner extends DeliteMultiArrayTestbenchRunner with Remove1DTest
trait Remove1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}.mutable
    vec.forIndices{i => val j = vec.length - i - 1; if (j % 2 == 1) vec.remove(j) }
    println(vec.mkString())
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == i*2)}
    mkReport
  }
}

object FlatMapDARunner extends DeliteMultiArrayTestbenchRunner with FlatMapDATest
trait FlatMapDATest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = DeliteArray.fromFunction(10){i => i}
    val v2 = vec.flatMap{x => DeliteArray.fromFunction(x+1){i => i} }
    collect(v2.reduce({(a,b) => a + b}, unit(0)) == 165)
    mkReport
  }
}

class DeliteMultiArraySuite extends DeliteSuite {
  // Passed tests
  //def testNew1D() { compileAndTest(New1DRunner) }
  //def testNew1DNull() { compileAndTest(New1DNullRunner) }
  //def testFromFunction1D() { compileAndTest(FromFunction1DRunner) }
  //def testUpdate1D() { compileAndTest(Update1DRunner) }
  //def testMap1D() { compileAndTest(Map1DRunner) }
  //def testZip1D() { compileAndTest(Zip1DRunner) }
  //def testReduce1D() { compileAndTest(Reduce1DRunner) }
  //def testForeach1D() { compileAndTest(Foreach1DRunner) }
  //def testForeachNoEffect1D() { compileAndTest(ForeachNoEffect1DRunner) }
  //def testForIndices1D() { compileAndTest(ForIndices1DRunner) }
  //def testForIndicesNoEffect1D() { compileAndTest(ForIndicesNoEffect1DRunner) }
  //def testPermute1D() { compileAndTest(Permute1DRunner) }
  //def testFilter() { compileAndTest(FilterRunner) }
  // Tests to be run:
  
  def testFlatMapDA() { compileAndTest(FlatMapDARunner) }

  //def testFlatMap() { compileAndTest(FlatMapRunner) }
  //def testSort() { compileAndTest(SortRunner) }
  //def testStringSplit() { compileAndTest(StringSplitRunner) }
  //def testInsert() { compileAndTest(InsertRunner) }
  //def testAppend() { compileAndTest(AppendRunner) }
  //def testInsertAll1D() { compileAndTest(InsertAll1DRunner) }
  //def testRemove1D() { compileAndTest(Remove1DRunner) }

  //def testSinglyNestedMultiArray() { compileAndTest(SinglyNestedMultiArrayRunner) }
  //def testNullMultiArray() { compileAndTest(NullMultiArrayRunner) }
  //def testMultiArrayUpdate() { compileAndTest(MultiArrayUpdateRunner) }
  //def testIllegalMultiArrayUpdate() { compileAndTest(IllegalMultiArrayUpdateRunner) }
  //def testIllegalMutation() { compileAndTest(IllegalMutationRunner) }
} 
