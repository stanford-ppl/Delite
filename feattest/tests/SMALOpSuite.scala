package feattest.tests.smal
import feattest.smal._
import feattest.DSLTestBenchmarks

// --- Miscellaneous tests

/* Test creation of nested MultiArray collections */
object MultiArraySinglyNestedTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => Array1D[Float](3)}
    vec foreach {k => collect(k.reduce(0.0f){(a,b) => a + b} == 0.0f)}
  }
}

/* Test rank discovery of nested MultiArray collections */
object MultiArrayUpdateTest extends SMALTest {
  def test() {
    val vec = Array1D[MultiArray[Int]](3)
    vec forIndices {i => vec(i) = Array1D.fromFunction(3){j => j}}
    collect(vec(0).reduce(0){_+_} == 3)
  }
}

/* Test error detection for mixing child ranks */
// TODO: Should expect an exception here - look up how to do this in Scala Suite
object MultiArrayIllegalUpdateTest extends SMALTest {
  def test() {
    val vec = Array1D[MultiArray[Int]](3)
    vec forIndices {i => vec(i) = Array1D.fromFunction(3){j => j}}
    // Update disallowed - all nested arrays must have same dimensionality
    vec(1) = Array2D.fromFunction(2, 2){(i,j) => i + j}
    collect(vec(0).reduce(0){_+_} == 3)
  }
}

/* Test that an immutable MultiArray can't be updated */
object MultiArrayIllegalMutationTest extends SMALTest {
  def test() {
    val vec = Array2D.fromFunction(2, 2){(i,j) => i + j}
    // Not allowed - vec is immutable
    vec(0,0) = 10
    collect(vec(0,0) == 0)
  }
}

/* Test the behavior of array with ambiguous arity */
object MultiArrayAmbiguousTest extends SMALTest {
  def test() {
    // Elements never initialized, so should be treated here as 1D (is this safe?)
    val vec = Array1D[MultiArray[Int]](3)
    collect(vec.mkString() == "null,null,null")
  }
}

/* Test Array1D creation */
object Array1DNewTest extends SMALTest {
  def test() {
    val vec = Array1D[Int](3)
    collect( vec.mkString() == "0,0,0" )
  }
}

/* Test rank detection of nested Array1Ds without initialization */
/* Should still be able to infer DeliteArray1D[DeliteArray1D[Int]] with rank info */
object Array1DNewNullTest extends SMALTest {
  def test() {
    val vec = Array1D[Array1D[Int]](3)
    collect( vec.mkString() == "null,null,null" )
  }
}

object Array1DFromFunctionTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(3){i => i}
    collect( vec.mkString() == "0,1,2" )
  }
}

object Array1DUpdateTest extends SMALTest {
  def test() {
    val vec = Array1D[Array1D[Int]](3)
    vec(0) = Array1D.fromFunction(3){i => i}
    collect( vec(0).mkString() == "0,1,2" )
  }
}

object Array1DMapTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(5){i => i}
    val vec2 = vec.map{x => x * 2}
    println(vec.mkString())
    println(vec2.mkString())
    collect(vec2(4) == 8)
  }
}

object Array1DZipTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(5){i => i}
    val vec2 = vec.map{x => vec.length - x}
    val vec3 = vec2.zip(vec){(a,b) => a + b}
    println(vec.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3(4) == 5)
  }
}

object Array1DReduceTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(5){i => i}
    val v = vec.reduce(0){(a,b) => a + b}
    collect(v == 10)
  }
}

object Array1DForeachTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.map{i => i * 20}
    vec2.foreach{k => println(vec2(k/20) + ", " + k) }
    vec2.foreach{k => collect(vec2(k/20) == k) }
  }
}

object Array1DForeachTest2 extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => 10 - i}
    vec.foreach{x => val v = Array1D[Int](x); v(0); println(x) }
    println(vec.mkString())
    collect(vec.reduce(0){(a,b) => a + b} == 55)
  }
}

object Array1DForIndicesTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.map{i => i * 20}
    vec2.forIndices{i => collect(vec2(i) == i*20) }
  }
}

object Array1DForIndicesTest2 extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => 10 - i}
    vec.forIndices{i => val v = Array1D[Int](i + 1); v(0); println(i) }
    collect(vec.reduce(0){(a,b) => a + b} == 55)
  }
}

/* Permuting a 1D array is equivalent to a copy, just confirming that here */
object Array1DPermuteTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = dmultia_permute(vec, Seq(0))
    vec.forIndices{i => collect(vec2(i) == vec(i)) }
  }
}

object Array1DFilterTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.filter{x => x % 2 == 0}
    collect(vec2.reduce(0){(a,b) => a + b} == 20)
  }
}

/* Test FlatMap */
/* TODO: FlatMap-Reduce fusion seems to be broken right now. Confirm on wip-master */
object Array1DFlatMapTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.flatMap{x => Array1D.fromFunction(x){i => i} }
    println(vec2.mkString())
    collect(vec2(3) == 0)
  }
}

object Array1DSortTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val vec2 = vec.sort
    val vec3 = vec.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(vec.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3(0) == 9 && vec2(0) == 0)
  }
}

object Array1DSplitStringTest extends SMALTest {
  def test() {
    val str = "4,3,2,1,0"
    val vec = Array1D.splitString(str, ",").map{_.toInt}
    println(vec.mkString())
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == 4 - i) }
  }
}

object Array1DInsertTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    vec.insert(0, -1)
    collect(vec.length == 5)
    for (i <- 0 until 5) { collect(vec(i) == i - 1) }
  }
}
object Array1DAppendTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    vec := 4
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == i) }
  }
}

object Array1DInsertAllTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(4){i => i}.mutable
    val vec2 = Array1D.fromFunction(4){i => i + 4}
    println(vec.mkString())
    vec ::= vec2
    println(vec.mkString())
    collect(vec.length == 8)
    vec.forIndices{i => collect(vec(i) == i) }
  }
}

object Array1DRemoveTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}.mutable
    for (i <- unit(0) until vec.length) {
      val j = vec.length - i - 1
      if (j % 2 == 1) vec.remove(j)
    }
    println(vec.mkString())
    collect(vec.length == 5)
    vec.forIndices{i => collect(vec(i) == i * 2) }
  }
}
object Array1DReshapeTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(16){i => i}
    val mat = vec.reshape(4, 4)
    println(mat.mkString())
    mat.forIndices{(i,j) => collect(mat(i,j) == 4*i + j) }
  }
}

object Array1DBufferNewTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) { buf.insert(0, i) }
    val vec = buf.map{k => 9 - k}
    println(buf.mkString())
    println(vec.mkString())
    vec.forIndices{i => collect(vec(i) == i) }
  }
}

object Array1DBufferZipTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) { buf(i) = 10 - i }
    val vec = buf.map{k => 9 - k}
    val vec2 = buf.zip(vec){(a,b) => a + b}
    println(buf.mkString())
    println(vec.mkString())
    println(vec2.mkString())
    vec2.forIndices{i => collect(vec2(i) == 9) }
  }
}

object Array1DBufferReduceTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(5){i => i + 1}.mutable
    val vec = Array1D.fromFunction(5){i => i + 6}

    buf ::= vec
    val prod = buf.reduce(1){(a,b) => a * b}
    println(prod)
    collect(prod == 3628800)
  }
}

object Array1DBufferForeachTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    val vec = Array1D.fromFunction(10){i => i * 2}
    buf ::= vec
    buf.foreach{k =>
      println(k + ": ")
      if (k < 10) {
        val x = buf(k) == 2*k
        println(x)
        collect(x)
      }
      else println(" [out of bounds]")
    }
  }
}

object Array1DBufferForeachTest2 extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    val vec = Array1D.fromFunction(10){i => 10 - i}
    buf ::= vec
    buf.foreach{x => val v = Array1D[Int](x); v(0); println(x)}
    collect(buf.reduce(0){(a,b) => a + b} == 55)
  }
}

object Array1DBufferFilterTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}
    buf ::= vec
    println(buf.mkString())
    val vec2 = buf.filter{x => x % 2 == 0}
    collect(vec2.reduce(0){_+_} == 40)
  }
}

object Array1DBufferFlatMapTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}
    buf ::= vec
    val vec2 = buf.flatMap{x => Array1D.fromFunction(x){i => i} }
    println(vec2.mkString())
    collect(vec2.length == 90)
    collect(vec2(3) == 0)
  }
}

object Array1DBufferMapToBufferTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) { buf := i }
    val buf2 = buf.map{k => 10 - k}.mutable
    buf2 ::= buf
    println(buf.mkString())
    println(buf2.mkString())
    collect(buf2.reduce(0){_+_} == 100)
  }
}

object Array1DBufferZipToBufferTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 10) { buf := i }
    val buf2 = Array1D.fromFunction(9){i => i}.mutable
    buf2 := 9
    val buf3 = buf.zip(buf2){(a,b) => a + b}.mutable
    buf3 ::= buf
    buf3 ::= buf2
    println(buf.mkString())
    println(buf2.mkString())
    println(buf3.mkString())
    collect(buf3.reduce(0){_+_} == 180)
  }
}

object Array1DBufferFilterToBufferTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}
    buf ::= vec
    val buf2 = buf.filter{x => x % 2 == 0}.mutable
    buf2.remove(1)
    println(buf2.mkString())
    collect(buf2.reduce(0){_+_} == 38)
  }
}

object Array1DBufferFlatMapToBufferTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    val vec = Array1D.fromFunction(10){i => 9 - i}
    buf ::= vec
    val buf2 = buf.flatMap{x => Array1D.fromFunction(x){i => i} }.mutable
    buf2.remove(0)
    buf2.remove(43)
    println(buf2.mkString())
    collect(buf2.length == 88)
    collect(buf2(3) == 1)
  }
}

/* Permuting a 1D array is just a copy, confirming that it works that way here */
object Array1DBufferPermuteTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    buf := 10
    val vec = dmultia_permute(buf, Seq(0))
    buf.forIndices{i => collect(buf(i) == vec(i)) }
  }
}

object Array1DBufferReshapeTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(16){i => i}.mutable
    buf ::= Array1D.fromFunction(4){i => 16 + i}
    val mat = buf.reshape(5, 4)
    println(mat.mkString())
    mat.forIndices{(i,j) => collect(mat(i,j) == 4*i + j) }
  }
}

object Array1DBufferSortTest extends SMALTest {
  def test() {
    val buf = Array1D.fromFunction(10){i => i}.mutable
    buf ::= buf
    val vec2 = buf.sort
    val vec3 = buf.sortWith{(a,b) => if (a < b) 1 else - 1 }
    println(buf.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3(0) == 9 && vec3(1) == 9 && vec2(0) == 0 && vec2(1) == 0)
  }
}

object Array1DSliceTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val view = vec.slice(0, 2, 5)
    collect(view.length == 5)
    for (i <- 0 until 5) { collect(vec(i) == 2*i) }
    println(view.mkString())
  }
}

object Array1DSliceTest2 extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val view = vec.slice(1, 2, 5)
    collect(view.length == 5)
    for (i <- 0 until 5) { collect(vec(i) == 2*i + 1) }
    println(view.mkString())
  }
}

object Array1DSliceOfSliceTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val view = vec.slice(1, 2, 10)     // 1 3 5 7 9 11 13 15 17 19 (index = 2i + 1)
    val view2 = view.slice(1, 3, 3)    // 3 9 15                   (index = 6i + 3)
    collect(view2.length == 3)
    for (i <- 0 until 3) { collect(view2(i) == 6*i + 3) }
    println(vec.mkString())
    println(view.mkString())
    println(view2.mkString())
  }
}

object Array1DViewPermuteTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val view = vec.slice(1, 2, 8) // odds 1 to 15
    val vec2 = view.permute(0)
    println(view.mkString())
    println(vec2.mkString())
    vec2.forIndices{i => collect(view(i) == vec2(i)) }
  }
}

object Array1DViewReshapeTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val view = vec.slice(1, 2, 8)   // odds 1 to 15
    val mat = view.reshape(2, 4)
    println(mat.mkString())
    mat.forIndices{(i,j) => collect(mat(i,j) == 8*i + 2*j + 1) }
  }
}

object Array1DViewMapTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val view = vec.slice(1, 2, 10)
    val vec2 = view.map{x => x - 1}
    println(view.mkString())
    println(vec2.mkString())
    vec2.forIndices{i => collect(vec2(i) == 2*i) }
  }
}

object Array1DViewZipTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val view = vec.slice(1, 2, 10)
    val view2 = vec.slice(0, 2, 10)
    val vec2 = view.zip(view2){_+_}
    vec2.forIndices{i => collect(vec2(i) == 4*i + 1) }
  }
}

object Array1DViewForeachTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val vec2 = Array1D[Int](10)
    val view = vec.slice(1, 2, 10)
    view.foreach{x => if (x < vec2.length) vec2(x) = x }

    println(vec2.mkString())
    vec2.forIndices{i => if (i % 2 == 0) collect(vec2(i) == 0) else collect(vec2(i) == i) }
  }
}

object Array1DViewForIndicesTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val vec2 = Array1D[Int](10)
    val view = vec.slice(1,2,10)
    view.forIndices{i => vec2(i) = view(i) }

    println(vec2.mkString())
    vec2.forIndices{i => collect(vec2(i) == 2*i + 1) }
  }
}

object Array1DViewFilterTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(20){i => i}
    val view = vec.slice(1, 2, 10)
    val vec2 = view.filter{x => x < 5 || x > 8 }
    println(vec2.mkString())
    collect(vec2(0) == 1)
    collect(vec2(1) == 3)
    collect(vec2(2) == 9)
  }
}

object Array1DViewFlatMapTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val view = vec.slice(1, 2, 5)
    val vec2 = view.flatMap{x => Array1D.fromFunction(x){i => i} }

    println(vec2.mkString())
    collect(vec2.length == 25)
    collect(vec2(3) == 2)
  }
}

object Array1DViewFlatMapOfViewTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val view = vec.slice(1, 2, 5)
    val vec2 = view.flatMap{x => val v = Array1D.fromFunction(x+1){i => i}; v.slice(1,2,v.length/2) }
    println(vec2.mkString())
    collect(vec2.length == 15)
    collect(vec2(5) == 5)
  }
}

object Array1DViewSortTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val view = vec.slice(1, 2, 5)
    val vec2 = view.sort
    val vec3 = view.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(vec.mkString())
    println(vec2.mkString())
    println(vec3.mkString())
    collect(vec3.length == 5 && vec2.length == 5)
    collect(vec3(0) == 9 && vec2(0) == 1)
  }
}

object Array1DBufferSliceTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 10)    // 2i + 1
    val view2 = view.slice(1, 2, 5)   // 4i + 3
    println(view2.mkString())
    collect(view2.length == 5)
    for (i <- 0 until 5) { collect(view2(i) == 4*i + 3) }
  }
}

object Array1DBufferViewPermuteTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 8) // odds 1 to 15
    val vec = view.permute(0)

    println(view.mkString())
    println(vec.mkString())
    vec.forIndices{i => collect(view(i) == vec(i)) }
  }
}

object Array1DBufferViewMapTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 10)
    val vec = view.map{x => x + 1}

    println(view.mkString())
    println(vec.mkString())
    vec.forIndices{i => collect(vec(i) == 2*i) }
  }
}

object Array1DBufferViewZipTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 10)
    val view2 = buf.slice(0, 2, 10)
    val vec = view.zip(view2){_+_}
    vec.forIndices{i => collect(vec(i) == 4*i + 1) }
  }
}

object Array1DBufferViewReduceTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 10)
    val x = view.reduce(0){_+_}
    collect(x == 100)
  }
}

object Array1BufferViewForeachTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 10)
    val vec = Array1D[Int](10)
    view.foreach{x => if (x < vec.length) vec(x) = x }
    println(vec.mkString())
    vec.forIndices{i => if (i % 2 == 0) collect(vec(i) == 0) else collect(vec(i) == i) }
  }
}

object Array1DBufferViewForIndicesTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 10)
    val vec = view.filter{x => x < 5 || x > 8 }
    println(vec.mkString())
    collect(vec(0) == 1)
    collect(vec(1) == 3)
    collect(vec(2) == 9)
  }
}

object Array1DBufferViewFlatMapTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 5)
    val vec = view.flatMap{x => Array1D.fromFunction(x){i => i} }
    println(vec.mkString())
    collect(vec.length == 25)
    collect(vec(3) == 2)
  }
}

object Array1DBufferViewFlatMapOfBufferViewTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 5) // 1, 3, 5, 7, 9
    val vec = view.flatMap{x =>
      val v = Array1D.fromFunction(x+1){i => i}.mutable
      v := 3
      v.slice(1,2,v.length/2)
    }
    println(vec.mkString())
    collect(vec.length == 15)
    collect(vec(5) == 5)
  }
}

object Array1DBufferViewSortTest extends SMALTest {
  def test() {
    val buf = Array1D[Int](0)
    for (i <- 0 until 20) { buf := i }
    val view = buf.slice(1, 2, 5)
    val vec = view.sort
    val vec2 = view.sortWith{(a,b) => if (a < b) 1 else -1 }
    println(view.mkString())
    println(vec.mkString())
    println(vec2.mkString())
    collect(vec2.length == 5 && vec.length == 5)
    collect(vec2(0) == 9 && vec(0) == 1)
  }
}

object Array2DNewTest extends SMALTest {
  def test() {
    val mat = Array2D[Int](3,3)
    mat(1,1) = 1
    println(mat.mkString())
    collect(mat.nRows == 3 && mat.nCols == 3 && mat.size == 9)
    collect(mat.mkString() == "0,0,0\n0,1,0\n0,0,0")
  }
}

object Array2DFromFunctionTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(2,3){(i,j) => i + j}
    println(mat.mkString())
    collect(mat.nRows == 2 && mat.nCols == 3 && mat.size == 6)
    collect(mat.mkString() == "0,1,2\n1,2,3")
  }
}

object Array2DApplyTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    println(mat.mkString())
    for (i <- unit(0) until mat.nRows)
      for (j <- unit(0) until mat.nCols)
        collect(mat(i,j) == i + j)
  }
}

object Array2DUpdateTest extends SMALTest {
  def test() {
    val mat = Array2D[Int](3,3)
    for (i <- unit(0) until mat.nRows)
      for (j <- unit(0) until mat.nCols)
        mat(i,j) = (i + 1) * (j + 1)

    for (i <- unit(0) until mat.nRows)
      for (j <- unit(0) until mat.nCols)
        collect(mat(i,j) == (i + 1) * (j + 1))
  }
}

object Array2DPermuteTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + (j*2) }
    val mat2 = mat.t

    println("mat: "); println(mat.mkString())
    println("mat2: "); println(mat2.mkString())
    for (i <- 0 until 3)
      for (j <- 0 until 3)
        collect(mat(i,j) == mat2(j,i))
  }
}

object Array2DReshapeTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    val vec = mat.reshape(9)
    collect(vec.mkString() == "0,1,2,1,2,3,2,3,4")
  }
}

object Array2DForIndicesTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    val mat2 = Array2D[Int](3,3)
    mat.forIndices{(i,j) => mat2(i,j) = mat(i,j) }

    for (i <- 0 until 3)
      for (j <- 0 until 3)
        collect(mat2(i,j) == mat(i,j))
  }
}

object Array2DForeachTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i*3 + j}
    println(mat.mkString())

    val vec = Array1D[Int](12)    // Bug in fusion here - vec and mat need to have different sizes
    mat.foreach{k => vec(k) = 2}  // otherwise they're incorrectly fused (TODO: Verify)

    println(vec.mkString())
    val sum = vec.reduce(0){_+_}
    println("sum = " + sum)
    collect(sum == 18)
  }
}

object Array2DMapTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    val mat2 = mat.map{_ * 2}
    println("mat: "); println(mat.mkString())
    println("mat2: "); println(mat2.mkString())

    mat2.forIndices{(i,j) => collect(mat2(i,j) == 2*mat(i,j)) }
  }
}

object Array2DZipTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}   // 0,1,2 ; 1,2,3 ; 2,3,4
    val mat2 = Array2D.fromFunction(3,3){(i,j) => i * j}  // 0,0,0 ; 0,1,2 ; 0,2,4
    val mat3 = mat.zip(mat2){(a,b) => a + b}              // 0,1,2 ; 1,3,5 ; 2,5,8

    println(mat3.mkString())
    collect(mat3.mkString() == "0,1,2\n1,3,5\n2,5,8")
  }
}

object Array2DReduceTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    collect(mat.fold(0){_+_} == 18)
  }
}

object Array2DMapRowsTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    val mat2 = mat.mapRows{row => val s = row.reduce(0){_+_}; row.map{_* s} }

    println("mat:"); println(mat.mkString())
    println("mat2:"); println(mat2.mkString())
    collect(mat2.mkString() == "0,3,6\n6,12,18\n18,27,36")
  }
}

object Array2DSlice2DTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(5,5){(i,j) => i + j} // 0,1,2,3,4 ; 1,2,3,4,5 ; 2,3,4,5,6 ; 3,4,5,6,7 ; 4,5,6,7,8
    val view = mat.slice(2,3,2,3)
    println("mat:"); println(mat.mkString())
    println("view:"); println(view.mkString())
    collect(view.nRows == 3 && view.nCols == 3 && view.size == 9)
    collect(view.mkString() == "4,5,6\n5,6,7\n6,7,8")
  }
}

object Array2DSlice2DTest2 extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(5,5){(i,j) => i + j}   // 0,1,2,3,4 ; 1,(2),3,(4),5 ; 2,3,4,5,6 ; 3,(4),5,(6),7 ; 4,5,6,7,8
    val view = mat.slice(1,2,2,1,2,2)

    println("mat:"); println(mat.mkString())
    println("view:"); println(view.mkString())
    collect(view.nRows == 2 && view.nCols == 2 && view.size == 4)
    collect(view.mkString() == "2,4\n4,6")
  }
}

object Array2DSlice1DTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(5,5){(i,j) => i + j*2}
    val row2 = mat.sliceRow(2)
    val col2 = mat.sliceCol(2)

    println(mat.mkString())
    println(row2.mkString())
    println(col2.mkString())

    collect(row2.mkString() == "2,4,6,8,10")
    collect(col2.mkString() == "4,5,6,7,8")
  }
}

object Array2DViewApplyTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(2,3,2,3) // 3x3 window centered at (3,3)

    println(view.mkString())

    collect(view.nRows == 3 && view.nCols == 3 && view.size == 9)

    for (i <- unit(0) until view.nRows)
      for (j <- unit(0) until view.nCols)
        collect(view(i,j) == i + j + 4)
  }
}

object Array2DViewPermuteTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + (j*2)}
    val view = mat.slice(2,3,2,3) // 3x3 window centered at (3,3)
    val mat2 = view.t

    println("view:"); println(view.mkString())
    println("transpose:"); println(mat2.mkString())

    for (i <- 0 until 3)
      for (j <- 0 until 3)
        collect(view(i,j) == mat2(j,i))
  }
}

object Array2DViewReshapeTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(2,3,2,3) // 3x3 window centered at (3,3)
    val vec = view.reshape(9)

    println(view.mkString())

    val str =  vec.mkString(",")
    println(str)
    collect(str == "4,5,6,5,6,7,6,7,8")
  }
}

object Array2DViewForIndicesTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val mat2 = Array2D[Int](3,3)

    val view = mat.slice(2,3,2,3) // 3x3 window centered at (3,3)

    view.forIndices{(i,j) => mat2(i,j) = view(i,j)} // copy window

    println("window:"); println(view.mkString())
    println("matrix:"); println(mat2.mkString())

    collect(mat2.mkString(";", ",") == "4,5,6;5,6,7;6,7,8")
  }
}

object Array2DViewForeachTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i*3 + j}
    val view = mat.slice(2,3,2,3) // 3x3 window centered at (3,3)

    val vec = Array1D[Int](101) // fusion bug (see above)
    view.foreach{k => vec(k) = 2}

    val sum = vec.reduce(0){_+_}
    println("sum = " + sum)
    collect(sum == 18)
  }
}

object Array2DViewMapTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(2,3,2,3) // 3x3 window centered at (3,3)

    val mat2 = view.map{_ * 2}

    println("view:"); println(view.mkString())
    println("mat2:"); println(mat2.mkString())

    mat2.forIndices{(i,j) => collect(mat2(i,j) == 2*view(i,j))}
  }
}

object Array2DViewZipTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(2,3,2,3)  // 3x3 window centered at (3,3)
    val view2 = mat.slice(4,3,6,3)  // 3x3 window centered at (5,7)

    val mat2 = view.zip(view2){(a,b) => a + b}

    println(mat2.mkString())
    collect(mat2.mkString(";",",") == "14,16,18;16,18,20;18,20,22")
  }
}

object Array2DViewReduceTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(2,3,2,3)  // 3x3 window centered at (3,3)

    collect(view.reduce(0){_+_} == 54)
  }
}

object Array2DViewSlice2DTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(2,7,2,7) // 7x7 window centered at (5,5)
    val view2 = view.slice(2,3,2,3) // 3x3 window centered at (3,3) in 7x7 window (actually at (5,5) in orig)

    println("mat:"); println(mat.mkString())
    println("view:"); println(view.mkString())
    println("view2:"); println(view2.mkString())

    collect(view2.nRows == 3 && view2.nCols == 3 && view2.size == 9)
    collect(view2.mkString() == "8,9,10\n9,10,11\n10,11,12")
  }
}

object Array2DViewSlice2DTest2 extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j}
    val view = mat.slice(1,2,5,1,2,5)
    val view2 = view.slice(1,2,2,1,2,2)

    println("mat:"); println(mat.mkString())
    println("view:"); println(view.mkString())
    println("view2:"); println(view2.mkString())

    collect(view2.nRows == 2 && view2.nCols == 2 && view2.size == 4)
    collect(view2.mkString() == "6,10\n10,14")
  }
}

object Array2DViewSlice1DTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j*2}
    val view = mat.slice(1,2,5,1,2,5)

    val row2 = view.sliceRow(2)
    val col2 = view.sliceCol(2)

    println("mat:"); println(mat.mkString())
    println("view:"); println(view.mkString())
    println("row2:"); println(row2.mkString())
    println("col2:"); println(col2.mkString())
                            //  1 3  5  7  9
    collect(row2.mkString() == "7,11,15,19,23")   // 5th row, odd columns
    collect(col2.mkString() == "11,13,15,17,19")  // 5th col, odd rows
  }
}

object Array2DViewMapRowsTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(10,10){(i,j) => i + j*2}
    val view = mat.slice(2,3,2,3)

    val mat2 = view.mapRows{r => val s = r.reduce(0){_+_}; r.map{_* s} }

    println("view:"); println(view.mkString())
    println("mat2:"); println(mat2.mkString())
    collect(mat2.mkString() == "60,75,90\n90,108,126\n126,147,168")
  }
}

object MkStringFuncTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFunction(3,3){(i,j) => i + j}
    val matStr = mat.mkStringFunc(";",","){e => unit("") + e}

    println(matStr)
    val vec = Array1D.fromFunction(10){i => i}
    val vecStr = vec.mkStringFunc(","){e => "x"}
    println(vecStr)

    val buf = Array1D[Int](0)
    buf ::= vec

    val bufStr = buf.mkStringFunc(","){e => unit("") + e}
    println(bufStr)

    val view = vec.slice(1,2,5)
    val viewStr = view.mkStringFunc("-"){e => unit("") + (unit(10) - e)}
    println(viewStr)

    val bufView = buf.slice(1,2,5)
    val bufViewStr = bufView.mkStringFunc(","){e => unit("") + e}
    println(bufViewStr)

    collect(matStr == "0,1,2;1,2,3;2,3,4")
    collect(vecStr == "x,x,x,x,x,x,x,x,x,x")
    collect(bufStr == "0,1,2,3,4,5,6,7,8,9")
    collect(viewStr == "9-7-5-3-1")
    collect(bufViewStr == "1,3,5,7,9")
  }
}

object MkStringFuncShortTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}
    val buf = Array1D[Int](0)
    buf ::= vec

    val bufStr = buf.mkStringFunc(","){e => unit("") + e}
    println(bufStr)

    collect(bufStr == "0,1,2,3,4,5,6,7,8,9")
  }
}

object Array1DReadTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFile("/home/david/PPL/data/vec.txt"){s => s}
    val cmp = Array1D.fromFunction(10){i => i + 1}.map{"" + _}
    println(vec.mkString())
    println(cmp.mkString())
    collect(vec.zip(cmp){(a,b) => a == b}.reduce(true){_&&_})
  }
}

object Array2DReadTest extends SMALTest {
  def test() {
    val mat = Array2D.fromFile("/home/david/PPL/data/mat.txt"){s => s}
    val cmp = Array2D.fromFunction(5,5){(i,j) => i + j + 1}.map{"" + _}
    println(mat.mkString())
    println(cmp.mkString())
    collect(mat.zip(cmp){(a,b) => a == b}.reduce(true){_&&_})
  }
}

object BranchAliasTest extends SMALTest {
  def test() {
    val vec = Array1D.fromFunction(10){i => i}

    val vec2 = if (vec.reduce(0){_+_} > 0) {
      Array1D.fromFunction(10){i => i}
    }
    else {
      val buf = Array1D[Int](0)
      buf ::= vec
      buf
    }
    println(vec2.mkString())
    collect(vec2.mkString() == "0,1,2,3,4,5,6,7,8,9")
  }
}

class SMALTests extends DSLTestBenchmarks {
  /*// --- Misc.
  def test1() { runTest(MultiArraySinglyNestedTest) }
  def test2() { runTest(MultiArrayUpdateTest) }
  def test3() { runTest(MultiArrayIllegalUpdateTest) }
  def test4() { runTest(MultiArrayIllegalMutationTest) }
  def test5() { runTest(MultiArrayAmbiguousTest) }
  def test89() { runTest(MkStringFuncTest) }
  def test90() { runTest(MkStringFuncShortTest) }

  // --- Reading
  def test91() { runTest(Array1DReadTest) }
  def test92() { runTest(Array2DReadTest) }

  // --- Array1D
  def test6() { runTest(Array1DNewTest) }
  def test7() { runTest(Array1DNewNullTest) }
  def test8() { runTest(Array1DFromFunctionTest) }
  def test9() { runTest(Array1DUpdateTest) }
  def test10() { runTest(Array1DMapTest) }
  def test11() { runTest(Array1DZipTest) }
  def test12() { runTest(Array1DReduceTest) }
  def test13() { runTest(Array1DForeachTest) }
  def test14() { runTest(Array1DForeachTest2) }
  def test15() { runTest(Array1DForIndicesTest) }
  def test16() { runTest(Array1DForIndicesTest2) }
  def test17() { runTest(Array1DPermuteTest) }
  def test18() { runTest(Array1DFilterTest) }
  def test19() { runTest(Array1DFlatMapTest) }
  def test20() { runTest(Array1DSortTest) }
  def test21() { runTest(Array1DSplitStringTest) }
  def test22() { runTest(Array1DInsertTest) }
  def test23() { runTest(Array1DAppendTest) }
  def test24() { runTest(Array1DInsertAllTest) }
  def test25() { runTest(Array1DRemoveTest) }
  def test26() { runTest(Array1DReshapeTest) }

  // --- Array1DBuffer
  def test27() { runTest(Array1DBufferNewTest) }
  def test28() { runTest(Array1DBufferZipTest) }
  def test29() { runTest(Array1DBufferReduceTest) }
  def test30() { runTest(Array1DBufferForeachTest) }
  def test31() { runTest(Array1DBufferForeachTest2) }
  def test32() { runTest(Array1DBufferFilterTest) }
  def test33() { runTest(Array1DBufferFlatMapTest) }
  def test34() { runTest(Array1DBufferMapToBufferTest) }
  def test35() { runTest(Array1DBufferZipToBufferTest) }
  def test36() { runTest(Array1DBufferFilterToBufferTest) }
  def test37() { runTest(Array1DBufferFlatMapToBufferTest) }
  def test38() { runTest(Array1DBufferPermuteTest) }
  def test39() { runTest(Array1DBufferReshapeTest) }

  // --- Array1DView
  def test40() { runTest(Array1DSliceTest) }
  def test41() { runTest(Array1DSliceTest2) }
  def test42() { runTest(Array1DSliceOfSliceTest) }
  def test43() { runTest(Array1DViewPermuteTest) }
  def test44() { runTest(Array1DViewReshapeTest) }
  def test45() { runTest(Array1DViewMapTest) }
  def test46() { runTest(Array1DViewZipTest) }
  def test47() { runTest(Array1DViewForeachTest) }
  def test48() { runTest(Array1DViewForIndicesTest) }
  def test49() { runTest(Array1DViewFilterTest) }
  def test50() { runTest(Array1DViewFlatMapOfViewTest) }
  def test51() { runTest(Array1DViewSortTest) }

  // --- Array1DBufferView
  def test52() { runTest(Array1DBufferSliceTest) }
  def test53() { runTest(Array1DBufferViewPermuteTest) }
  def test54() { runTest(Array1DBufferViewMapTest) }
  def test55() { runTest(Array1DBufferViewZipTest) }
  def test56() { runTest(Array1DBufferViewReduceTest) }
  def test57() { runTest(Array1BufferViewForeachTest) }
  def test58() { runTest(Array1DBufferViewForIndicesTest) }
  def test59() { runTest(Array1DBufferViewFlatMapTest) }
  def test60() { runTest(Array1DBufferViewFlatMapOfBufferViewTest) }
  def test61() { runTest(Array1DBufferViewSortTest) }

  // --- Array2D
  def test62() { runTest(Array2DNewTest) }
  def test63() { runTest(Array2DFromFunctionTest) }
  def test64() { runTest(Array2DApplyTest) }
  def test65() { runTest(Array2DUpdateTest) }
  def test66() { runTest(Array2DPermuteTest) }
  def test67() { runTest(Array2DReshapeTest) }
  def test68() { runTest(Array2DForIndicesTest) }
  def test69() { runTest(Array2DForeachTest) }
  def test70() { runTest(Array2DMapTest) }
  def test71() { runTest(Array2DZipTest) }
  def test72() { runTest(Array2DReduceTest) }
  def test73() { runTest(Array2DMapRowsTest) }

  // --- Array2DView
  def test74() { runTest(Array2DSlice2DTest) }
  def test75() { runTest(Array2DSlice2DTest2) }
  def test76() { runTest(Array2DSlice1DTest) }
  def test77() { runTest(Array2DViewApplyTest) }
  def test78() { runTest(Array2DViewPermuteTest) }
  def test79() { runTest(Array2DViewReshapeTest) }
  def test80() { runTest(Array2DViewForIndicesTest) }
  def test81() { runTest(Array2DViewForeachTest) }
  def test82() { runTest(Array2DViewMapTest) }
  def test83() { runTest(Array2DViewZipTest) }
  def test84() { runTest(Array2DViewReduceTest) }
  def test85() { runTest(Array2DViewSlice2DTest) }
  def test86() { runTest(Array2DViewSlice2DTest2) }
  def test87() { runTest(Array2DViewSlice1DTest) }
  def test88() { runTest(Array2DViewMapRowsTest) }*/

  // --- Wrapping tests
  def test93() { runTest(BranchAliasTest) }
}
