import simplevector.compiler._
import simplevector.library._
import simplevector.shared._

object HelloSimpleCompiler extends SimpleVectorApplicationCompiler with HelloSimple
object HelloSimpleInterpreter extends SimpleVectorApplicationInterpreter with HelloSimple

trait HelloSimple extends SimpleVectorApplication {
  def main() = {
    println("hello world")

    val v1 = Vector[Int](10)
    val v2 = Vector[Int](10)

    // defs
    var i = 0
    while (i < v1.length) {
      v1(i) = i*2
      v2(i) = i
      println("v1(" + i + "): " + v1(i))
      println("v2(" + i + "): " + v2(i))
      i += 1
    }

    // zip
    val v3 = v1+v2
    i = 0
    println("v3 = v1+v2")
    while (i < v3.length) {
      println("v3(" + i + "): " + v3(i))
      i += 1
    }

    // single
    println("v4 = v3.slice(3,5)")
    val v4 = v3.slice(3,5)
    i = 0
    while (i < v4.length) {
      println("v4(" + i + "): " + v4(i))
      i += 1
    }

    // map
    println("v5 = v4*5")
    val v5 = v4*5
    i = 0
    while (i < v5.length) {
      println("v5(" + i + "): " + v5(i))
      i += 1
    }

    // reduce
    println("v5.sum:")
    val z = v5.sum
    println(z)

    // foreach
    println("v1.pprint:")
    v1.pprint

    // map,reduce,mapreduce
    val vc = v1.map(e => e+2)
    println("vc.pprint:")
    vc.pprint
    val vc2 = vc.mapreduce(e => e-1, (a,b) => a+b)
    println("vc2: " + vc2)

    // filter
    // println("v6 = v1.filter(_ < 5)")
    // val v6 = v1.filter(_ < 5)
    // v6.pprint

    // groupbyreduce
    println("v7 = v2.groupByReduce(e => e % 2 == 0, e => e*10, (a,b) => a+b)")
    val v7 = v2.groupByReduce[Boolean,Int](e => e % 2 == 0, e => e*10, (a,b) => a+b)
    // should be 2 elements (one for even group, one for odd group)
    println(v7(true))
    println(v7(false))

    // groupby
    println("v8 = v2.groupBy(e => e % 2 == 0, e => e*10)")
    val v8 = v2.groupBy(e => e % 2 == 0, e => e*10)
    i = 0
    while (i < v8.length) {
      println("group " + i + ": ")
      v8(i).pprint
      i += 1
    }

    // flatmap
    println("v9 = v2.flatMap(e => Vector[Int](5)")
    val v9 = v2.flatMap(e => Vector[Int](5))
    println("v9.length: " + v9.length)
    println("v9: ")
    v9.pprint

    // foo
    val fooResult = foo[Int](i => i+100, 2, 13, (a,b) => a + 5, 0.7, d => d)
    // // a0 == a1 == ... an = 107
    // // z == 0.7
    // // y = 15
    println("fooResult: " + fooResult)
  }

}
