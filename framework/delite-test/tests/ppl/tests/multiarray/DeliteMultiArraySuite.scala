package ppl.tests.multiarray

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._
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
    // FlatMap-Reduce fusion is apparently broken right now
    //collect(v2.reduce(unit(0)){(a,b) => a + b} == 165)
    println(v2.mkString())
    collect(v2(3) == 0)
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
    collect(vec3(0) == 9 && vec2(0) == 0)
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
    vec.forIndices{i => collect(vec(i) == 4 - i)}
    mkReport
  }
}

object InsertRunner extends DeliteMultiArrayTestbenchRunner with InsertTest
trait InsertTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    vec.insert(0, -1)
    collect(vec.length == 5)
    for (i <- 0 until 5)
      collect(vec(i) == i - 1)
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

    println(vec.mkString())
    val vec2 = Array1D.fromFunction(4){i => i + 4}
    vec ::= vec2
    println(vec.mkString())
    collect(vec.length == 8)
    vec.forIndices{i => collect(vec(i) == i)}
    mkReport
  }
}

object Remove1DRunner extends DeliteMultiArrayTestbenchRunner with Remove1DTest
trait Remove1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(10){i => i}.mutable
    for (i <- 0 until vec.length) {
      val j = vec.length - i - 1
      if (j % 2 == 1) vec.remove(j)   // shouldn't have remove in a parallel loop
    }
    println(vec.mkString())
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == i*2)}
    mkReport
  }
}

object Reshape1DRunner extends DeliteMultiArrayTestbenchRunner with Reshape1DTest
trait Reshape1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = Array1D.fromFunction(16){i => i}
    val mat = vec.reshape(4,4)
    println(mat.mkString())
    mat.forIndices{(i,j) => collect(mat(i,j) == 4*i + j) }
    mkReport
  }
}

object Buffer1DNewRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DNewTest
trait Buffer1DNewTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) {
      buf.insert(0, i)
    }
    val vec = buf.map{k => 9 - k}
    println(buf.mkString())
    println(vec.mkString())
    vec.forIndices{i => collect(vec(i) == i)}
    mkReport
  }
}

object Buffer1DZipRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DZipTest
trait Buffer1DZipTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D[Int](10)
    for (i <- 0 until 10)
      buf(i) = 10 - i
    val vec = buf.map{k => 9 - k}

    val vec2 = buf.zip(vec){(a,b) => a + b}

    println(buf.mkString())
    println(vec.mkString())
    println(vec2.mkString())

    vec2.forIndices{i => collect(vec2(i) == 9) }
    mkReport
  }
}

object Buffer1DReduceRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DReduceTest
trait Buffer1DReduceTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(5){i => i+1}.mutable
    val vec = Array1D.fromFunction(5){i => i+6}

    buf ::= vec 
    val prod = buf.reduce{1}{_*_}
    println(prod)
    collect(prod == 3628800)
    mkReport
  }
}

object Buffer1DForeachRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DForeachTest
trait Buffer1DForeachTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D[Int](0)
    val vec = Array1D.fromFunction(10){i => i * 2}
    buf ::= vec
    buf.foreach{k => 
      print(k + ": ")
      if (k < 10) {
        val x = buf(k) == 2*k
        println(x)
        collect(x) 
      }
      else 
        println(" [out of bounds]")
    }
    mkReport
  }
}

object Buffer1DForeachNoEffectRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DForeachTest
trait Buffer1DForeachNoEffectTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D[Int](0)
    val vec = Array1D.fromFunction(10){i => 10 - i}
    buf ::= vec
    buf.foreach{x => val v = Array1D[Int](x); v(0); println(x) }
    collect(buf.reduce(0){_+_} == 55)
    mkReport 
  }
}

object BufferFilterRunner extends DeliteMultiArrayTestbenchRunner with BufferFilterTest
trait BufferFilterTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}
    buf ::= vec
    println(buf.mkString())

    val vec2 = buf.filter{x => x % 2 == 0}
    collect(vec2.reduce(0){_+_} == 40)
    mkReport
  }
}

object BufferFlatMapRunner extends DeliteMultiArrayTestbenchRunner with BufferFlatMapTest
trait BufferFlatMapTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}

    buf ::= vec
    val vec2 = buf.flatMap{x => Array1D.fromFunction(x){i => i} }
    // FlatMap-Reduce fusion is apparently broken right now
    //collect(v2.reduce(unit(0)){(a,b) => a + b} == 165)
    println(vec2.mkString())
    collect(vec2.length == 90)
    collect(vec2(3) == 0)
    mkReport
  }
}

// buffer flatMap which has a body that returns a buffer
object BufferFlatMapBufferRunner extends DeliteMultiArrayTestbenchRunner with BufferFlatMapBufferTest
trait BufferFlatMapBufferTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}

    buf ::= vec
    val vec2 = buf.flatMap{x => val z = Array1D.fromFunction(x){i => i}.mutable; z ::= z;  (z) }
    // FlatMap-Reduce fusion is apparently broken right now
    //collect(v2.reduce(unit(0)){(a,b) => a + b} == 165)
    println(vec2.mkString())
    collect(vec2.length == 180)
    collect(vec2(3) == 1)
    mkReport
  }
}


object Buffer1DMapToBufferRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DMapToBufferTest
trait Buffer1DMapToBufferTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) { buf := i }

    val buf_out = buf.map{k => 10 - k}.mutable

    buf_out ::= buf

    println(buf.mkString())
    println(buf_out.mkString())

    collect(buf_out.reduce(0){_+_} == 100)
    mkReport
  }
}

object Buffer1DZipToBufferRunner extends DeliteMultiArrayTestbenchRunner with Buffer1DZipToBufferTest
trait Buffer1DZipToBufferTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) { buf := i }

    val buf2 = Array1D.fromFunction(9){i => i}.mutable
    buf2 := 9

    val buf_out = buf.zip(buf2){(a,b) => a + b}.mutable

    buf_out ::= buf
    buf_out ::= buf2

    println(buf.mkString())
    println(buf2.mkString())
    println(buf_out.mkString())

    collect(buf_out.reduce(0){_+_} == 180)
    mkReport
  }
}

object BufferFilterToBufferRunner extends DeliteMultiArrayTestbenchRunner with BufferFilterToBufferTest
trait BufferFilterToBufferTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}
    buf ::= vec

    val buf_out = buf.filter{x => x % 2 == 0}.mutable
    buf_out.remove(1)
    println(buf_out.mkString())
    collect(buf_out.reduce(0){_+_} == 38)
    mkReport
  }
}

object BufferFlatMapToBufferRunner extends DeliteMultiArrayTestbenchRunner with BufferFlatMapToBufferTest
trait BufferFlatMapToBufferTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}

    buf ::= vec
    val buf_out = buf.flatMap{x => Array1D.fromFunction(x){i => i} }.mutable
    buf_out.remove(0)
    buf_out.remove(43)
    // FlatMap-Reduce fusion is apparently broken right now
    //collect(v2.reduce(unit(0)){(a,b) => a + b} == 165)
    println(buf_out.mkString())
    collect(buf_out.length == 88)
    collect(buf_out(3) == 1)
    mkReport
  }
}

// Permuting a 1D array is just a copy, just confirming that it works here
object BufferPermute1DRunner extends DeliteMultiArrayTestbenchRunner with BufferPermute1DTest
trait BufferPermute1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    buf := 10
    val vec = dmultia_permute(buf, Seq(0))
    buf.forIndices{i => collect(buf(i) == vec(i)) }
    mkReport
  }
}

object BufferReshape1DRunner extends DeliteMultiArrayTestbenchRunner with BufferReshape1DTest
trait BufferReshape1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(16){i => i}.mutable
    buf ::= Array1D.fromFunction(4){i => 16 + i}
    val mat = buf.reshape(5,4)
    println(mat.mkString())
    mat.forIndices{(i,j) => collect(mat(i,j) == 4*i + j) }
    mkReport
  }
}

object BufferSortRunner extends DeliteMultiArrayTestbenchRunner with BufferSortTest
trait BufferSortTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    buf ::= buf
    val vec2 = buf.sort
    val vec3 = buf.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(buf.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3(0) == 9 && vec3(1) == 9 && vec2(0) == 0 && vec2(0) == 0)
    mkReport
  }
} 

object DeliteBufferMapToBufferRunner extends DeliteMultiArrayTestbenchRunner with DeliteBufferMapToBufferTest
trait DeliteBufferMapToBufferTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buf = DeliteArrayBuffer[Int](0)
    for (i <- 0 until 10) { buf += i }

    val buf_out = buf.map{k => 10 - k}.mutable

    buf_out ++= DeliteArray.fromFunction(10){i => 10 - i}

    for (i <- 0 until buf.length) { print(buf(i) + ",")}
    println("")
    for (i <- 0 until buf_out.length) {print(buf_out(i) + ",") }
    println("")

    collect(buf_out.reduce{_+_}(0) == 110)
    mkReport
  }
}

object Slice1DRunner extends DeliteMultiArrayTestbenchRunner with Slice1DTest 
trait Slice1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(10){i => i}
    val view = arr.slice(0, 2, 5)

    collect(view.length == 5)
    collect(view(0) == 0)
    collect(view(1) == 2)
    collect(view(2) == 4)
    collect(view(3) == 6)
    collect(view(4) == 8)
    println(view.mkString())

    mkReport
  }
}

object Slice1DOffsetRunner extends DeliteMultiArrayTestbenchRunner with Slice1DOffsetTest 
trait Slice1DOffsetTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(10){i => i}
    val view = arr.slice(1, 2, 5)

    collect(view.length == 5)
    collect(view(0) == 1)
    collect(view(1) == 3)
    collect(view(2) == 5)
    collect(view(3) == 7)
    collect(view(4) == 9)
    println(view.mkString())

    mkReport
  }
}

object Slice1DSliceRunner extends DeliteMultiArrayTestbenchRunner with Slice1DSliceTest 
trait Slice1DSliceTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 10)     // 1 3 5 7 9 11 13 15 17 19 (index = 2i + 1)
    val view2 = view.slice(1, 3, 3)    // 3 9 15                   (index = 6i + 3)

    collect(view2.length == 3)
    collect(view2(0) == 3)
    collect(view2(1) == 9)
    collect(view2(2) == 15)
    println(arr.mkString())
    println(view.mkString())
    println(view2.mkString())
    mkReport
  }
}

object View1DPermuteRunner extends DeliteMultiArrayTestbenchRunner with View1DPermuteTest
trait View1DPermuteTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 8)  // odds 1 to 15

    val arr2 = view.permute(0)

    println(view.mkString())
    println(arr2.mkString())
    arr2.forIndices{i => collect(view(i) == arr2(i)) }
    mkReport
  }
}

object View1DReshapeRunner extends DeliteMultiArrayTestbenchRunner with View1DReshapeTest
trait View1DReshapeTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 8)   // odds 1 to 15

    val mat = view.reshape(2, 4)
    println(mat.mkString())

    mat.forIndices{(i,j) => collect(mat(i,j) == 8*i + 2*j + 1) }
    mkReport
  }
}

object View1DMapRunner extends DeliteMultiArrayTestbenchRunner with View1DMapTest
trait View1DMapTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 10)

    val arr2 = view.map{x => x - 1}

    println(view.mkString())
    println(arr2.mkString())

    arr2.forIndices{i => collect(arr2(i) == 2*i)}
    mkReport
  }
}

object View1DZipRunner extends DeliteMultiArrayTestbenchRunner with View1DZipTest
trait View1DZipTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 10)
    val view2 = arr.slice(0, 2, 10)

    val arr2 = view.zip(view2){_+_}

    arr2.forIndices{i => collect(arr2(i) == 4*i + 1)}
    mkReport
  }
}

object View1DReduceRunner extends DeliteMultiArrayTestbenchRunner with View1DReduceTest
trait View1DReduceTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 10)

    val x = view.reduce(0){_+_}
    collect(x == 100)
    mkReport
  }
}

object View1DForeachRunner extends DeliteMultiArrayTestbenchRunner with View1DForeachTest
trait View1DForeachTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val arr2 = Array1D[Int](10)

    val view = arr.slice(1, 2, 10)
    view.foreach{x => if (x < arr2.length) arr2(x) = x } 

    println(arr2.mkString())

    arr2.forIndices{i => if (i % 2 == 0) collect(arr2(i) == 0) else collect(arr2(i) == i)}
    mkReport
  }
}

object View1DForIndicesRunner extends DeliteMultiArrayTestbenchRunner with View1DForIndicesTest
trait View1DForIndicesTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val arr2 = Array1D[Int](10)
    val view = arr.slice(1,2,10)

    view.forIndices{i => arr2(i) = view(i) }

    println(arr2.mkString())
    arr2.forIndices{i => collect(arr2(i) == 2*i + 1)}
    mkReport
  }
}

object View1DFilterRunner extends DeliteMultiArrayTestbenchRunner with View1DFilterTest 
trait View1DFilterTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(20){i => i}
    val view = arr.slice(1, 2, 10)

    val arr2 = view.filter{x => x < 5 || x > 8}

    println(arr2.mkString())
    collect(arr2(0) == 1)
    collect(arr2(1) == 3)
    collect(arr2(2) == 9)
    mkReport
  }
}

object View1DFlatMapRunner extends DeliteMultiArrayTestbenchRunner with View1DFlatMapTest 
trait View1DFlatMapTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(10){i => i}
    val view = arr.slice(1, 2, 5)

    val arr2 = view.flatMap{x => Array1D.fromFunction(x){i => i}}
    println(arr2.mkString())

    collect(arr2.length == 25)
    collect(arr2(3) == 2)
    //collect(arr2.reduce(0){_+_} == 95)
    mkReport
  }
}

object View1DFlatMapViewRunner extends DeliteMultiArrayTestbenchRunner with View1DFlatMapViewTest
trait View1DFlatMapViewTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(10){i => i}
    val view = arr.slice(1, 2, 5)

    val arr2 = view.flatMap{x => val v = Array1D.fromFunction(x+1){i => i}; v.slice(1,2,v.length/2) }
    println(arr2.mkString())

    collect(arr2.length == 15)
    collect(arr2(5) == 5)
    //collect(arr2.reduce(0){_+_} == 55)
    mkReport
  }
}

object View1DSortRunner extends DeliteMultiArrayTestbenchRunner with View1DSortTest 
trait View1DSortTest extends DeliteMultiArrayTestbench {
  def main() = {
    val arr = Array1D.fromFunction(10){i => i}
    val view = arr.slice(1,2,5)

    val vec2 = view.sort
    val vec3 = view.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(view.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3.length == 5 && vec2.length == 5)
    collect(vec3(0) == 9 && vec2(0) == 1)
    mkReport
  }
}

// --- 1D BufferView

object SliceBuffer1DRunner extends DeliteMultiArrayTestbenchRunner with SliceBuffer1DTest
trait SliceBuffer1DTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }
    
    val view = buff.slice(1, 2, 5)

    println(view.mkString())
    collect(view.length == 5)
    collect(view(0) == 1)
    collect(view(1) == 3)
    collect(view(2) == 5)
    collect(view(3) == 7)
    collect(view(4) == 9)
    mkReport
  }
}

object BufferView1DSliceRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DSliceTest
trait BufferView1DSliceTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 10)    // 2i + 1
    val view2 = view.slice(1, 2, 5)   // 4i + 3

    println(view2.mkString())
    collect(view2.length == 5)
    collect(view2(0) == 3)
    collect(view2(1) == 7)
    collect(view2(2) == 11)
    collect(view2(3) == 15)
    collect(view2(4) == 19)
    mkReport
  }
}

object BufferView1DPermuteRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DPermuteTest
trait BufferView1DPermuteTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 8)  // odds 1 to 15

    val arr2 = view.permute(0)

    println(view.mkString())
    println(arr2.mkString())
    arr2.forIndices{i => collect(view(i) == arr2(i)) }
    mkReport
  }
}

object BufferView1DReshapeRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DReshapeTest
trait BufferView1DReshapeTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 8)   // odds 1 to 15

    val mat = view.reshape(2, 4)
    println(mat.mkString())

    mat.forIndices{(i,j) => collect(mat(i,j) == 8*i + 2*j + 1) }
    mkReport
  }
}

object BufferView1DMapRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DMapTest
trait BufferView1DMapTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 10)

    val arr2 = view.map{x => x - 1}

    println(view.mkString())
    println(arr2.mkString())

    arr2.forIndices{i => collect(arr2(i) == 2*i)}
    mkReport
  }
}

object BufferView1DZipRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DZipTest
trait BufferView1DZipTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 10)
    val view2 = buff.slice(0, 2, 10)

    val arr2 = view.zip(view2){_+_}

    arr2.forIndices{i => collect(arr2(i) == 4*i + 1)}
    mkReport
  }
}

object BufferView1DReduceRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DReduceTest
trait BufferView1DReduceTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 10)

    val x = view.reduce(0){_+_}
    collect(x == 100)
    mkReport
  }
}

object BufferView1DForeachRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DForeachTest
trait BufferView1DForeachTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 10)

    val arr2 = Array1D[Int](10)
    view.foreach{x => if (x < arr2.length) arr2(x) = x } 

    println(arr2.mkString())

    arr2.forIndices{i => if (i % 2 == 0) collect(arr2(i) == 0) else collect(arr2(i) == i)}
    mkReport
  }
}

object BufferView1DForIndicesRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DForIndicesTest
trait BufferView1DForIndicesTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1,2,10)

    val arr2 = Array1D[Int](10)
    view.forIndices{i => arr2(i) = view(i) }

    println(arr2.mkString())
    arr2.forIndices{i => collect(arr2(i) == 2*i + 1)}
    mkReport
  }
}

object BufferView1DFilterRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DFilterTest 
trait BufferView1DFilterTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 10)

    val arr2 = view.filter{x => x < 5 || x > 8}

    println(arr2.mkString())
    collect(arr2(0) == 1)
    collect(arr2(1) == 3)
    collect(arr2(2) == 9)
    mkReport
  }
}

object BufferView1DFlatMapRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DFlatMapTest 
trait BufferView1DFlatMapTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 5)

    val arr2 = view.flatMap{x => Array1D.fromFunction(x){i => i}}
    println(arr2.mkString())

    collect(arr2.length == 25)
    collect(arr2(3) == 2)
    //collect(arr2.reduce(0){_+_} == 95)
    mkReport
  }
}

object BufferView1DFlatMapBufferViewRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DFlatMapBufferViewTest
trait BufferView1DFlatMapBufferViewTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1, 2, 5)

    val arr2 = view.flatMap{x => val v = Array1D.fromFunction(x+1){i => i}; v.slice(1,2,v.length/2) }
    println(arr2.mkString())

    collect(arr2.length == 15)
    collect(arr2(5) == 5)
    //collect(arr2.reduce(0){_+_} == 55)
    mkReport
  }
}

object BufferView1DSortRunner extends DeliteMultiArrayTestbenchRunner with BufferView1DSortTest 
trait BufferView1DSortTest extends DeliteMultiArrayTestbench {
  def main() = {
    val buff = Array1D[Int](0)
    for (i <- unit(0) until unit(20)) { buff := i }

    val view = buff.slice(1,2,5)

    val vec2 = view.sort
    val vec3 = view.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(view.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3.length == 5 && vec2.length == 5)
    collect(vec3(0) == 9 && vec2(0) == 1)
    mkReport
  }
}

class DeliteMultiArraySuite extends DeliteSuite {
  // Passed tests

  def testNew1D() { compileAndTest(New1DRunner) }
  def testNew1DNull() { compileAndTest(New1DNullRunner) }
  def testFromFunction1D() { compileAndTest(FromFunction1DRunner) }
  def testUpdate1D() { compileAndTest(Update1DRunner) }
  def testMap1D() { compileAndTest(Map1DRunner) }
  def testZip1D() { compileAndTest(Zip1DRunner) }
  def testReduce1D() { compileAndTest(Reduce1DRunner) }
  def testForeach1D() { compileAndTest(Foreach1DRunner) }
  def testForeachNoEffect1D() { compileAndTest(ForeachNoEffect1DRunner) }
  def testForIndices1D() { compileAndTest(ForIndices1DRunner) }
  def testForIndicesNoEffect1D() { compileAndTest(ForIndicesNoEffect1DRunner) }
  def testPermute1D() { compileAndTest(Permute1DRunner) }
  def testFilter() { compileAndTest(FilterRunner) }
  def testFlatMap() { compileAndTest(FlatMapRunner) }
  def testStringSplit() { compileAndTest(StringSplitRunner) }
  def testInsert() { compileAndTest(InsertRunner) }
  def testAppend() { compileAndTest(AppendRunner) }
  def testInsertAll1D() { compileAndTest(InsertAll1DRunner) }
  def testRemove1D() { compileAndTest(Remove1DRunner) }
  def testReshape1D() { compileAndTest(Reshape1DRunner) }
  def testSort() { compileAndTest(SortRunner) }

  def testBuffer1DNew() { compileAndTest(Buffer1DNewRunner) }
  def testBuffer1DZip() { compileAndTest(Buffer1DZipRunner) }
  def testBuffer1DReduce() { compileAndTest(Buffer1DReduceRunner) }
  def testBuffer1DForeach() { compileAndTest(Buffer1DForeachRunner) }
  def testBuffer1DForeachNoEffect() { compileAndTest(Buffer1DForeachNoEffectRunner) }
  def testBufferFilter() { compileAndTest(BufferFilterRunner) }
  def testBufferFlatMap() { compileAndTest(BufferFlatMapRunner) }
  def testBufferPermute1D() { compileAndTest(BufferPermute1DRunner) }
  def testBufferReshape1D() { compileAndTest(BufferReshape1DRunner) }
  def testBufferSort() { compileAndTest(BufferSortRunner) }
  def testBufferFlatMapBuffer() { compileAndTest(BufferFlatMapBufferRunner) }

  def testSlice1D() { compileAndTest(Slice1DRunner) }
  def testSlice1DOffset() { compileAndTest(Slice1DOffsetRunner) }
  def testSlice1DSlice() { compileAndTest(Slice1DSliceRunner) }

  def testView1DPermute() { compileAndTest(View1DPermuteRunner) }
  def testView1DReshape() { compileAndTest(View1DReshapeRunner) }
  def testView1DMap() { compileAndTest(View1DMapRunner) }
  def testView1DZip() { compileAndTest(View1DZipRunner) }
  def testView1DReduce() { compileAndTest(View1DReduceRunner) }  

  def testView1DForeach() { compileAndTest(View1DForeachRunner) }
  def testView1DForIndices() { compileAndTest(View1DForIndicesRunner) }

  def testView1DFilter() { compileAndTest(View1DFilterRunner) }
  def testView1DFlatMap() { compileAndTest(View1DFlatMapRunner) }
  def testView1DFlatMapView() { compileAndTest(View1DFlatMapViewRunner) }
  
  def testView1DSort() { compileAndTest(View1DSortRunner) }

  def testSliceBuffer1D() { compileAndTest(SliceBuffer1DRunner) }
  def testBufferView1DSlice() { compileAndTest(BufferView1DSliceRunner) }
  def testBufferView1DPermute() { compileAndTest(BufferView1DPermuteRunner) }
  def testBufferView1DReshape() { compileAndTest(BufferView1DReshapeRunner) }
  def testBufferView1DMap() { compileAndTest(BufferView1DMapRunner) }
  def testBufferView1DZip() { compileAndTest(BufferView1DZipRunner) }
  def testBufferView1DReduce() { compileAndTest(BufferView1DReduceRunner) }  

  def testBufferView1DForeach() { compileAndTest(BufferView1DForeachRunner) }
  def testBufferView1DForIndices() { compileAndTest(BufferView1DForIndicesRunner) }

  def testBufferView1DFilter() { compileAndTest(BufferView1DFilterRunner) }
  def testBufferView1DFlatMap() { compileAndTest(BufferView1DFlatMapRunner) }
  def testBufferView1DFlatMapBufferView() { compileAndTest(BufferView1DFlatMapBufferViewRunner) }
  
  def testBufferView1DSort() { compileAndTest(BufferView1DSortRunner) }
  

  // Tests passing, still need to figure out supposed aliasing error
  //def testBuffer1DMapToBuffer() { compileAndTest(Buffer1DMapToBufferRunner) }       // illegal sharing btwn insertAll, .mutable (map)
  //def testBuffer1DZipToBuffer() { compileAndTest(Buffer1DZipToBufferRunner) }       // illegal sharing btwn insertAll, .mutable (map)
  //def testBufferFilterToBuffer() { compileAndTest(BufferFilterToBufferRunner) }     // illegal sharing btwn remove, .mutable (map)
  //def testBufferFlatMapToBuffer() { compileAndTest(BufferFlatMapToBufferRunner) }   // illegal sharing btwn remove, .mutable (map)

  //def testDeliteBufferMapToBuffer() { compileAndTest(DeliteBufferMapToBufferRunner) }
  //def testSinglyNestedMultiArray() { compileAndTest(SinglyNestedMultiArrayRunner) }
  //def testNullMultiArray() { compileAndTest(NullMultiArrayRunner) }
  //def testMultiArrayUpdate() { compileAndTest(MultiArrayUpdateRunner) }
  //def testIllegalMultiArrayUpdate() { compileAndTest(IllegalMultiArrayUpdateRunner) }
  //def testIllegalMutation() { compileAndTest(IllegalMutationRunner) }
} 
